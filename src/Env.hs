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
  , insertInStoreT, maxUnwrapType
  , readPrim
  , freshAddr, insertInHeap, readObject, replaceObject, numberOfRefs
  , Slot(..)
  , randomString
  ) where

import qualified Data.Map.Strict as M
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Monad.IO.Class      ( MonadIO(..) )

import Lang.Abs (Ident, Type(..))
import Value    (Value(..), Closure, TClosure, Mutability, Mutability( Imm,Mut ), Object, Color, Slot(..), Addr(..), VarInfo (..))
import Data.Set
import Data.Int
import System.Random (StdGen, randomR, mkStdGen)
import Data.Traversable (mapAccumL)

data Env v f = Env
  { scopes :: [M.Map Ident (v, Mutability {-, MutableBorrows-})]
  , funs :: M.Map Ident (f, Mutability)
  , heap :: M.Map Addr Object
  , nextA :: Addr  -- to remove, use length of heap instead
  , refStore :: [M.Map Addr (Ident, v, Mutability {-reference mutability-})] --, Mutability {-, MutableBorrows-})]
  }

empty :: Env v f
empty = Env [M.empty] M.empty M.empty (Addr 0) [M.empty]

type App env err a = StateT env (ExceptT err IO) a

runApp
  :: env
  -> App env err a
  -> IO (Either err (a, env))
runApp st act = runExceptT (runStateT act st)


type EvalEnv = Env (Slot Value) Closure
type Eval a  = App EvalEnv String a
runEval :: EvalEnv -> Eval a -> IO (Either String (a, EvalEnv))
runEval = runApp

type TCEnv  = Env VarInfo  TClosure
type TC a   = App TCEnv String a
runTC  :: TCEnv  -> TC a -> IO (Either String (a, TCEnv))
runTC = runApp

-- TC a = App (Env Type TClosure) String a

insertInStore :: (Ident, Slot Value, Mutability) -> Eval Addr
insertInStore tup = do
  en <- get
  let top = head $ refStore en
  let rest = tail $ refStore en
  let a@(Addr addr) = nextA en

  modify (\e -> e { 
    refStore = (M.insert a tup top):rest ,
    nextA = (Addr $ (addr + 1))
    })
  return a

getBorrowedValue :: Addr -> Eval (Slot Value)
getBorrowedValue (Addr addr) = do
  store <- gets refStore
  let (Just (id, val, _)) = M.lookup (Addr addr) (head store)
  return val

maxUnwrapBorrowedValue :: Value -> Eval Value
maxUnwrapBorrowedValue (VRef (Addr addr)) = do
  store <- gets refStore
  let (Just (id, Prim val, _)) = M.lookup (Addr addr) (head store)
  maxUnwrapBorrowedValue val
maxUnwrapBorrowedValue (VMutRef (Addr addr)) = do
  store <- gets refStore
  let (Just (id, Prim val, _)) = M.lookup (Addr addr) (head store)
  maxUnwrapBorrowedValue val
maxUnwrapBorrowedValue other = return other

modifyBorrowedValue :: Addr -> Slot Value -> Eval ()
modifyBorrowedValue (Addr addr) newVal = do
  store <- gets refStore
  let (Just (id, Prim val, mut)) = M.lookup (Addr addr) (head store)
  case mut of 
    Imm -> throwError $ "MODIFY BORROWED VALUE, THIS SHOULD NOT HAPPEN BECAUSE TYPECHECKER SHOULD FORBID MODIFYING IMMUTABLE AN REFERENCES' VALUE" ++
                        "Cannot modify value under immutable reference"
    Mut -> do
              modify (\env -> env { refStore = ((M.insert) (Addr addr) (id, newVal, mut) (head store)):(tail store) } )
              scs <- gets scopes
              foundId <- lookupVarMaybe id
              case foundId of
                (Just _) -> assignVar id newVal
                (Nothing) -> return ()

maxUnwrapType :: Type -> TC Type
maxUnwrapType (TRef t) = maxUnwrapType t
maxUnwrapType (TMutRef t) = maxUnwrapType t
maxUnwrapType t = return t 

numberOfRefs :: Type -> (Type, Int)
numberOfRefs (TRef t) = let tup = numberOfRefs t in (fst tup, snd tup + 1)
numberOfRefs (TMutRef t) = let tup = numberOfRefs t in (fst tup, snd tup + 1)
numberOfRefs t = (t, 0)

insertInStoreT :: (Ident, VarInfo, Mutability) -> TC Addr
insertInStoreT tup@(name, vi, mut) = do
  en <- get
  let top = head $ refStore en
  let rest = tail $ refStore en
  let a@(Addr addr) = nextA en
  let (VI ty cf live ib mb) = vi
  let newVI = if mut == Mut then (VI ty cf live ib (mb+1)) 
              else (VI ty cf live (ib+1) mb)

  modify (\e -> e { 
    refStore = (M.insert a tup top):rest ,
    nextA = (Addr $ (addr + 1))
    })
  return a

-- insertVarT :: Ident -> (VarInfo, Mutability) -> TC ()
-- insertVarT x tup = modify (\env -> 
--   let scope  = topScope env
--       scope' = M.insert x tup scope
--   in  env { scopes = scope':(tail (scopes env)) })

freshAddr :: Eval Addr
freshAddr = do
  env <- get
  let Addr a = nextA env
  modify (\e -> e { nextA = Addr (a+1) })
  return $ Addr a

insertInHeap :: Object -> Eval Addr
insertInHeap obj = do 
  a <- freshAddr
  modify (\env -> 
    let heap' = M.insert a obj (heap env)
    in
      env { heap = heap' }
    )
  return a

readObject :: Addr -> Eval Object
readObject a = do
  h <- gets heap 
  case M.lookup a h of
    Just o -> return o
    Nothing -> throwError $ "No element in heap with address " ++ show a

replaceObject :: Addr -> Object -> Eval ()
replaceObject a o = do
  h <- gets heap
  let h' = M.insert a o h
  modify (\e -> e { heap = h' })
  return ()

-- lookupSlot :: Ident -> Eval (Slot, Mutability)
-- lookupSlot x = do
--   env <- get
--   let iter [] = throwError $ "Variable " ++ show x ++ " is not bound"
--       iter (f:fs) = maybe (search fs) return $ M.lookup x f
--   search (scopes env)

-- pushes new empty scope
pushScope :: Env v f -> Env v f
pushScope env = env { scopes = M.empty : scopes env }

-- pops last scope
popScope :: Env v f -> Env v f
popScope env = env { scopes = tail (scopes env)}

-- pushVarInScope :: Ident -> (Value, Mutability) -> Env v f -> Env v f
-- pushVarInScope name tup env = env { scopes = }

-- executes 
withScope :: Eval a -> Eval a
withScope body = do
  modify (\e -> pushScope e)
  r <- body
  modify (\e -> popScope e)
  return r

withScopeT :: TC a -> TC a
withScopeT body = do
  modify (\e -> pushScope e)
  r <- body
  modify (\e -> popScope e)
  return r

-- withVarInScope :: Ident -> (Value, Mutability) -> Eval a -> Eval a
-- withVarInScope name tup body = do
--   modify (\e -> )

topScope :: Env v f -> M.Map Ident (v, Mutability)
topScope env = head (scopes env)

lookupVar :: Ident -> Eval (Slot Value, Mutability)
lookupVar x = do
  env <- get
  let scps = scopes env
      iter [] = throwError $ "Variable " ++ show x ++ " is not bound"
      iter (s:scs) = 
        case M.lookup x s of
          -- Just Moved -> throwError $ "Variable " ++ show x ++ " has had its value moved."
          Just vm -> return vm
          Nothing -> iter scs
  iter scps

lookupVarMaybe :: Ident -> Eval (Maybe (Slot Value, Mutability))
lookupVarMaybe x = do
  env <- get
  let scps = scopes env
      iter [] = return Nothing
      iter (s:scs) = 
        case M.lookup x s of
          -- Just Moved -> throwError $ "Variable " ++ show x ++ " has had its value moved."
          Just vm -> return (Just vm)
          Nothing -> iter scs
  iter scps

insertVar :: Ident -> (Slot Value, Mutability) -> Eval ()
insertVar x tup = modify (\env ->
  let scope  = topScope env
      scope' = M.insert x tup scope
  in  env { scopes = scope':(tail (scopes env)) })

readPrim :: Ident -> Eval (Value, Mutability)
readPrim x = do
  (slot, mut) <- lookupVar x
  case slot of
    Prim v -> pure (v, mut)
    Moved  -> throwError $ "Variable " ++ show x ++ " was moved"
    -- _      -> err "is not a primitive"
  
assignVar :: Ident -> Slot Value -> Eval ()
assignVar x slotVal = do
  fs <- gets scopes
  let iter _ [] = throwError $ "variable " ++ show x ++ " is not bound"
      iter pref (sc:scs) = 
        case M.lookup x sc of
          Just(_, Imm) -> throwError $ "variable " ++ show x ++ " is immutable"
          Just(_, Mut) -> do
            -- liftIO $ print "we are here"
            let newScope = modifyVar x (slotVal, Mut) pref sc scs
            -- liftIO $ print $ "newScope" ++ show newScope
            modify (\env -> env { scopes = newScope })
            -- scs <- gets scopes
            -- liftIO $ print $ "scope" ++ show scs
          Nothing -> iter (pref ++ [sc]) scs
  iter [] fs

modifyVar :: 
  Ident -> 
  (Slot Value, Mutability) -> 
  [M.Map Ident (Slot Value, Mutability)] -> 
  M.Map Ident (Slot Value, Mutability) -> 
  [M.Map Ident (Slot Value, Mutability)] -> 
  [M.Map Ident (Slot Value, Mutability)]
modifyVar n tup prefix curr rest = 
  case M.lookup n curr of
    Just _  -> prefix ++ (M.insert n tup curr : rest)
    Nothing ->
      case rest of
        [] -> error $ "variable " ++ show n ++ " is not bound"
        next:rest' -> modifyVar n tup (prefix ++ [curr]) next rest'

-- assignVarT :: Ident -> VarInfo -> TC ()
-- assignVarT x vi = do
--   fs <- gets scopes
--   let iter _ [] = throwError $ "variable " ++ show x ++ " is not bound"
--       iter pref (sc:scs) = 
--         case M.lookup x sc of
--           Just(_, Imm) -> throwError $ "variable " ++ show x ++ " is immutable"
--           Just(_, Mut) -> do
--             -- liftIO $ print "we are here"
--             let newScope = modifyVarT x (vi, Mut) pref sc scs
--             -- liftIO $ print $ "newScope" ++ show newScope
--             modify (\env -> env { scopes = newScope })
--             -- scs <- gets scopes
--             -- liftIO $ print $ "scope" ++ show scs
--           Nothing -> iter (pref ++ [sc]) scs
--   iter [] fs

changeVarTIB :: Ident -> Int -> TC ()
changeVarTIB x newImm = do
  fs <- gets scopes
  let iter _ [] = throwError $ "variable " ++ show x ++ " is not bound"
      iter pref (sc:scs) = 
        case M.lookup x sc of
          Just(oldVI, m) -> do
            let newScope = modifyVarT x (oldVI { immutableBorrows=newImm }, m) pref sc scs
            modify (\env -> env { scopes = newScope })
          Nothing -> iter (pref ++ [sc]) scs
  iter [] fs

changeVarTMB :: Ident -> Int -> TC ()
changeVarTMB x newMut = do
  fs <- gets scopes
  let iter _ [] = throwError $ "variable " ++ show x ++ " is not bound"
      iter pref (sc:scs) = 
        case M.lookup x sc of
          Just(oldVI, m) -> do
            let newScope = modifyVarT x (oldVI { mutableBorrows=newMut }, m) pref sc scs
            modify (\env -> env { scopes = newScope })
          Nothing -> iter (pref ++ [sc]) scs
  iter [] fs

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

insertVarT :: Ident -> (VarInfo, Mutability) -> TC ()
insertVarT x tup = modify (\env -> 
  let scope  = topScope env
      scope' = M.insert x tup scope
  in  env { scopes = scope':(tail (scopes env)) })

  -- modify $ \s -> s { vars = M.insert x p (vars s) }

lookupVarT :: Ident -> TC (VarInfo, Mutability)
lookupVarT x = do
  env <- get
  let scps = scopes env
      iter [] = throwError $ "Variable " ++ show x ++ " is not bound"
      iter (s:scs) = 
        case M.lookup x s of
          Just vm -> return vm
          Nothing -> iter scs
  iter scps

lookupFun :: Ident -> Eval (Closure, Mutability)
lookupFun f = do
  fs <- gets funs
  case M.lookup f fs of
    Just pair -> return pair
    Nothing   -> throwError $ "undefined function: " ++ show f

insertFun :: Ident -> (Closure, Mutability) -> Eval ()
insertFun f pair = modify $ \env -> env { funs = M.insert f pair (funs env) }

lookupFunT :: Ident -> TC (TClosure, Mutability)
lookupFunT f = do
  fs <- gets funs
  case M.lookup f fs of
    Just pair -> pure pair
    Nothing   -> throwError $ "undefined function: " ++ show f

insertFunT :: Ident -> (TClosure, Mutability) -> TC ()
insertFunT f p = modify $ \env -> env { funs = M.insert f p (funs env) }

allowedChars :: String
allowedChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_*-@#"

randomString :: StdGen -> (String, StdGen)
randomString gen =
  let len = length allowedChars
      pickChar g = let (i, g') = randomR (0, len - 1) g
                   in (g', allowedChars !! i)
      (gen', chars) = mapAccumL (\g _ -> pickChar g) gen [1..6]
  in (chars, gen')
