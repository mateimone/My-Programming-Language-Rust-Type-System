{-# LANGUAGE TemplateHaskell #-}

module Env
  ( Env(..)
  , Env.empty
  , App
  , runApp
  , MonadState, MonadError, MonadIO, throwError, modify, liftIO
  , Eval, runEval
  , TC,   runTC
  , withScope, lookupVar, insertVar, assignVar, lookupFun, insertFun, lookupVarMaybe
  , withScopeT, lookupVarT, insertVarT, changeVarTIB, changeVarTMB, lookupFunT, insertFunT
  , insertInStore, getBorrowedValue, maxUnwrapBorrowedValue, modifyBorrowedValue
  , maxUnwrapType
  , readPrim
  , logEval
  , freshAddr, insertInHeap, readObject, replaceObject, numberOfRefs
  , heapL, scopesL, funsL, nextAL, refStoreL
  , randomString
  ) where

import qualified Data.Map.Strict as M
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Monad.IO.Class      ( MonadIO(..) )
import Control.Lens

import Lang.Abs (Ident, Type(..), Light)
import Value    (Value(..), Closure, TClosure, Mutability, Mutability( Imm,Mut ), Object, Addr(..), VarInfo (..))
import Data.Set
import Data.Int
import System.Random (StdGen, randomR, mkStdGen)
import Data.Traversable (mapAccumL)
import FiniteMap
import Control.Concurrent


-- The mutable environment, used by both the interpreter (v=Value, f=Closure) and typechecker (v=VarInfo, f=TClosure)
data Env v f = Env
  { scopes :: [M.Map Ident (v, Mutability)]
  , funs :: M.Map Ident f
  , heap :: FiniteMap Addr Object
  , nextA :: Addr
  , refStore :: [M.Map Addr (Ident, v, Mutability)]
  , logChan :: Chan String
  }

-- Lenses creation for the environment's records
makeLensesFor
  [ ("scopes", "scopesL")
  , ("funs", "funsL")
  , ("heap", "heapL")
  , ("nextA", "nextAL")
  , ("refStore", "refStoreL")
  , ("logChan", "logChanL")
  ] ''Env

-- Empty environment creation
empty :: Chan String -> Env v f
empty ch = Env [M.empty] M.empty FiniteMap.empty (Addr 0) [M.empty] ch

-- Monad stack - StateT over ExceptT over IO
type App env err a = StateT env (ExceptT err IO) a

-- Run the source program, returning either a result or an error
runApp
  :: env
  -> App env err a
  -> IO (Either err (a, env))
runApp st act = runExceptT (runStateT act st)

-- Concrete instance for runtime
type EvalEnv = Env Value Closure
type Eval a = App EvalEnv String a
runEval :: EvalEnv -> Eval a -> IO (Either String (a, EvalEnv))
runEval = runApp

-- Concrete instance for typechecking
type TCEnv = Env VarInfo  TClosure
type TC a = App TCEnv String a
runTC  :: TCEnv -> TC a -> IO (Either String (a, TCEnv))
runTC = runApp

-- Write a message to the async log channel stored in the environment
-- A message is written only when a new element is placed in the heap or in the reference store
-- Messages are written to "runtime.log"
logEval :: String -> Eval ()
logEval s = do
  ch <- use logChanL
  liftIO (writeChan ch s)

-- Insert a new borrow in the reference store
insertInStore :: (Ident, Value, Mutability) -> Eval Addr
insertInStore tup = do
  store <- use refStoreL
  a@(Addr addr) <- use nextAL
  top <- use (refStoreL . _head)
  rest <- use (refStoreL . _tail)
  refStoreL .= (M.insert a tup top) : rest
  nextAL .= Addr (addr + 1)

  let (_, v, m) = tup
  let mut = case m of
              Imm -> "immutably"
              Mut -> "mutably"

  logEval $ "alloc store @" ++ show a ++ ", " ++ mut ++ " borrowed value is " ++ show v ++ " (at runtime)"
  return a

-- Get the borrowed value from the reference store given an address
getBorrowedValue :: Addr -> Eval Value
getBorrowedValue (Addr addr) = do
  store <- use refStoreL
  let iter [] = throwError $ "Address " ++ show addr ++ " cannot be found in the reference store"
      iter (s:scs) = 
        case M.lookup (Addr addr) s of
          (Just (id, val, _)) -> return val
          Nothing -> iter scs

  iter store

-- If a value sits behind multiple references, unwrap all references until getting to it
maxUnwrapBorrowedValue :: Value -> Eval Value
maxUnwrapBorrowedValue (VRef (Addr addr)) = do
  val <- getBorrowedValue (Addr addr)
  maxUnwrapBorrowedValue val
maxUnwrapBorrowedValue (VMutRef (Addr addr)) = do
  val <- getBorrowedValue (Addr addr)
  maxUnwrapBorrowedValue val
maxUnwrapBorrowedValue other = return other

-- Modify a value under a reference given an address
modifyBorrowedValue :: Addr -> Value -> Eval ()
modifyBorrowedValue (Addr addr) newVal = do
  top <- use (refStoreL . _head)
  rest <- use (refStoreL . _tail)
  let (Just (id, val, mut)) = M.lookup (Addr addr) top
  case mut of 
    Imm -> throwError $ "MODIFY BORROWED VALUE, THIS SHOULD NOT HAPPEN BECAUSE TYPECHECKER SHOULD FORBID MODIFYING IMMUTABLE AN REFERENCES' VALUE" ++
                        "Cannot modify value under immutable reference"
    Mut -> do
              modify (\env -> env { refStore = ((M.insert) (Addr addr) (id, newVal, mut) top):rest } )
              scs <- use scopesL
              foundId <- lookupVarMaybe id
              case foundId of
                (Just _) -> assignVar id newVal
                Nothing -> return ()

-- Peel all reference layers off of a type
maxUnwrapType :: Type -> TC Type
maxUnwrapType (TRef t) = maxUnwrapType t
maxUnwrapType (TMutRef t) = maxUnwrapType t
maxUnwrapType t = return t 

-- Peel all reference layers off of a type while also counting the number of layers
numberOfRefs :: Type -> (Type, Int)
numberOfRefs (TRef t) = let tup = numberOfRefs t in (fst tup, snd tup + 1)
numberOfRefs (TMutRef t) = let tup = numberOfRefs t in (fst tup, snd tup + 1)
numberOfRefs t = (t, 0)

-- Get a fresh address for a new element on the heap or in the reference store
freshAddr :: Eval Addr
freshAddr = do
  adr@(Addr a) <- use nextAL
  nextAL .= Addr (a + 1)
  
  return adr

-- Insert a new object on the heap
insertInHeap :: Object -> Eval Addr
insertInHeap obj = do 
  a <- freshAddr
  logEval $ "alloc heap @" ++ show a ++ ", object " ++ show obj
  heapL %= (\hp -> bind a obj hp)
  return a

-- Get an object from the heap given its address
readObject :: Addr -> Eval Object
readObject a = do
  h <- use heapL
  case lookupFM a h of
    Just o -> return o
    Nothing -> throwError $ "No element in heap with address " ++ show a

-- Replace an object on the heap at an address
replaceObject :: Addr -> Object -> Eval ()
replaceObject a o = do
  logEval $ "replace heap @" ++ show a ++ ", object " ++ show o
  heapL %= (\hp -> bind a o hp)

-- Discard borrows after a scope ends
releaseBorrows :: M.Map Addr (Ident, VarInfo, Mutability) -> TC ()
releaseBorrows frame = mapM_ cancelOneBorrow ls
  where
    ls = M.elems frame
    cancelOneBorrow (borrowedVar, _, borrowKind) = do
      (VI t l ib mb, _) <- lookupVarT borrowedVar

      case borrowKind of
        Imm -> changeVarTIB borrowedVar (ib - 1)
        Mut -> changeVarTMB borrowedVar (mb - 1)

-- pushes new empty variable and reference store scope
pushScope :: Env v f -> Env v f
pushScope env = env { scopes = M.empty : scopes env 
                    , refStore = M.empty : refStore env}

-- pops last variable and reference store scope
popScope :: Env v f -> Env v f
popScope env = env { scopes = tail (scopes env)
                   , refStore = tail (refStore env)}

-- Push a new variable and reference store scope, executes the body with those, then pops them
withScope :: Eval a -> Eval a
withScope body = do
  modify (\e -> pushScope e)
  r <- body
  modify (\e -> popScope e)
  return r

-- Push a new variable and reference store scope, infers the body with those, then pops them and adjusts borrows accordingly
withScopeT :: TC a -> TC a
withScopeT body = do
  modify (\e -> pushScope e)
  r <- body
  topStore <- use (refStoreL . _head)
  releaseBorrows topStore
  modify (\e -> popScope e)
  return r

-- Gets the top variable scope
topScope :: Env v f -> M.Map Ident (v, Mutability)
topScope env = head (env ^. scopesL)

-- Searches for a variable in all variable scopes
lookupVar :: Ident -> Eval (Value, Mutability)
lookupVar x = do
  scps <- use scopesL
  let iter [] = throwError $ "Variable " ++ show x ++ " is not bound"
      iter (s:scs) = 
        case M.lookup x s of
          Just vm -> return vm
          Nothing -> iter scs
  iter scps

-- Searches for a variable in all variable scopes without throwing an error if not found
lookupVarMaybe :: Ident -> Eval (Maybe (Value, Mutability))
lookupVarMaybe x = do
  scps <- use scopesL
  let iter [] = return Nothing
      iter (s:scs) = 
        case M.lookup x s of
          Just vm -> return (Just vm)
          Nothing -> iter scs
  iter scps
  
-- Insert a variable in the top variable scope
insertVar :: Ident -> (Value, Mutability) -> Eval ()
insertVar x tup = 
  modify (over (scopesL . _head) (M.insert x tup))

-- Read the value of a variable
readPrim :: Ident -> Eval (Value, Mutability)
readPrim x = do
  (slot, mut) <- lookupVar x
  case slot of
    v -> return (v, mut)

-- Modify the value of a variable
assignVar :: Ident -> Value -> Eval ()
assignVar x slotVal = do
  fs <- use scopesL
  let iter _ [] = throwError $ "variable " ++ show x ++ " is not bound"
      iter pref (sc:scs) = 
        case M.lookup x sc of
          Just(_, Imm) -> throwError $ "variable " ++ show x ++ " is immutable"
          Just(_, Mut) -> do
            let newScope = modifyVar x (slotVal, Mut) pref sc scs
            scopesL .= newScope
          Nothing -> iter (pref ++ [sc]) scs
  iter [] fs

-- Helper function for modifying the value of a variable
modifyVar :: 
  Ident -> 
  (Value, Mutability) -> 
  [M.Map Ident (Value, Mutability)] -> 
  M.Map Ident (Value, Mutability) -> 
  [M.Map Ident (Value, Mutability)] -> 
  [M.Map Ident (Value, Mutability)]
modifyVar n tup prefix curr rest = 
  case M.lookup n curr of
    Just _  -> prefix ++ (M.insert n tup curr : rest)
    Nothing ->
      case rest of
        [] -> error $ "variable " ++ show n ++ " is not bound"
        next:rest' -> modifyVar n tup (prefix ++ [curr]) next rest'

-- Modify the number of immutable borrows a variable has - used when going out of a scope
changeVarTIB :: Ident -> Int -> TC ()
changeVarTIB x newImm = do
  fs <- use scopesL
  let iter _ [] = throwError $ "variable " ++ show x ++ " is not bound"
      iter pref (sc:scs) = 
        case M.lookup x sc of
          Just(oldVI, m) -> do
            let newScope = modifyVarT x (oldVI { immutableBorrows=newImm }, m) pref sc scs
            scopesL .= newScope
          Nothing -> iter (pref ++ [sc]) scs
  iter [] fs

-- Modify the number of mutable borrows a variable has - used when going out of a scope
changeVarTMB :: Ident -> Int -> TC ()
changeVarTMB x newMut = do
  fs <- use scopesL
  let iter _ [] = throwError $ "variable " ++ show x ++ " is not bound"
      iter pref (sc:scs) = 
        case M.lookup x sc of
          Just(oldVI, m) -> do
            let newScope = modifyVarT x (oldVI { mutableBorrows=newMut }, m) pref sc scs
            scopesL .= newScope
          Nothing -> iter (pref ++ [sc]) scs
  iter [] fs

-- Helper function for modifying the variable information of a variable
modifyVarT :: 
  Ident -> 
  (VarInfo, Mutability) -> 
  [M.Map Ident (VarInfo, Mutability)] -> 
  M.Map Ident (VarInfo, Mutability) -> 
  [M.Map Ident (VarInfo, Mutability)] -> 
  [M.Map Ident (VarInfo, Mutability)]
modifyVarT n tup prefix curr rest = 
  case M.lookup n curr of
    Just _  -> prefix ++ (M.insert n tup curr : rest)
    Nothing ->
      case rest of
        [] -> error $ "variable " ++ show n ++ " is not bound"
        next:rest' -> modifyVarT n tup (prefix ++ [curr]) next rest'

-- Insert a variable in the top variable scope (typechecking variant)
insertVarT :: Ident -> (VarInfo, Mutability) -> TC ()
insertVarT x tup = 
  modify (over (scopesL . _head) (M.insert x tup))

-- Searches for a variable in all variable scopes (typechecking variant)
lookupVarT :: Ident -> TC (VarInfo, Mutability)
lookupVarT x = do
  scps <- use scopesL
  let iter [] = throwError $ "Variable " ++ show x ++ " is not bound"
      iter (s:scs) = 
        case M.lookup x s of
          Just vm -> return vm
          Nothing -> iter scs
  iter scps

-- Searches for a function in the function store
lookupFun :: Ident -> Eval Closure
lookupFun f = do
  fs <- use funsL
  case M.lookup f fs of
    Just pair -> return pair
    Nothing   -> throwError $ "undefined function: " ++ show f

-- Inserts a function in the function store
insertFun :: Ident -> Closure -> Eval ()
insertFun f pair = funsL %= M.insert f pair

-- Searches for a function in the function store (typechecking variant)
lookupFunT :: Ident -> TC TClosure
lookupFunT f = do
  fs <- use funsL
  case M.lookup f fs of
    Just pair -> return pair
    Nothing   -> throwError $ "undefined function: " ++ show f

-- Inserts a function in the function store (typechecking variant)
insertFunT :: Ident -> TClosure -> TC ()
insertFunT f p = funsL %= M.insert f p


-- Helper for generating the random string
allowedChars :: String
allowedChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_*-@#"

-- Generates a random string of 6 letters - used when creating references to values (not variables) 
-- to give a random name to the bound reference
randomString :: StdGen -> (String, StdGen)
randomString gen =
  let len = length allowedChars
      pickChar g = let (i, g') = randomR (0, len - 1) g
                   in (g', allowedChars !! i)
      (gen', chars) = mapAccumL (\g _ -> pickChar g) gen [1..6]
  in (chars, gen')
