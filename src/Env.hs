module Env
  ( Env(..)
  , Env.empty
  , App
  , runApp
  , MonadState, MonadError, MonadIO, throwError, gets, modify, liftIO
  , Eval, runEval
  , TC,   runTC
  , withScope, lookupVar, insertVar, assignVar, lookupFun, insertFun
  , withScopeT, lookupVarT, insertVarT, lookupFunT, insertFunT
  ) where

import qualified Data.Map.Strict as M
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Monad.IO.Class      ( MonadIO(..) )

import Lang.Abs (Ident, Type)
import Value    (Value, Closure, TClosure, Mutability, Mutability( Imm,Mut ))
import Data.Set


data Env v f = Env
  { scopes :: [M.Map Ident (v, Mutability)]
  , funs :: M.Map Ident (f, Mutability)
  }

empty :: Env v f
empty = Env [M.empty] M.empty

type App env err a = StateT env (ExceptT err IO) a

runApp
  :: env
  -> App env err a
  -> IO (Either err (a, env))
runApp st act = runExceptT (runStateT act st)


type EvalEnv = Env Value Closure
type Eval a  = App EvalEnv String a
runEval :: EvalEnv -> Eval a -> IO (Either String (a, EvalEnv))
runEval = runApp

type TCEnv  = Env Type  TClosure
type TC a   = App TCEnv String a
runTC  :: TCEnv  -> TC a -> IO (Either String (a, TCEnv))
runTC = runApp

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

topScope:: Env v f -> M.Map Ident (v, Mutability)
topScope env = head (scopes env)

lookupVar :: Ident -> Eval (Value, Mutability)
lookupVar x = do
  env <- get
  let scps = scopes env
      iter [] = throwError $ "Variable " ++ show x ++ " is not bound"
      iter (s:scs) = 
        case M.lookup x s of
          Just vm -> return vm
          Nothing -> iter scs
  iter scps

insertVar :: Ident -> (Value,Mutability) -> Eval ()
insertVar x tup = modify (\env ->
  let scope  = topScope env
      scope' = M.insert x tup scope
  in  env { scopes = scope':(tail (scopes env)) })
  
-- this could be passed to the typechecker instead
assignVar :: Ident -> Value -> Eval ()
assignVar x v = do
  fs <- gets scopes
  let iter _ [] = throwError $ "variable " ++ show x ++ " is not bound"
      iter pref (sc:scs) = 
        case M.lookup x sc of
          Just(_, Imm) -> throwError $ "variable " ++ show x ++ " is immutable"
          Just(_, Mut) -> do
            let newScope = modifyVar x (v, Mut) pref sc scs
            modify (\env -> env { scopes = newScope })
          Nothing -> iter (pref ++ [sc]) scs
  iter [] fs

modifyVar :: Ident -> (Value, Mutability) -> [M.Map Ident (Value, Mutability)] -> M.Map Ident (Value, Mutability) -> [M.Map Ident (Value, Mutability)] -> [M.Map Ident (Value, Mutability)]
modifyVar n tup prefix curr rest = 
  case M.lookup n curr of
    Just _  -> prefix ++ (M.insert n tup curr : rest)
    Nothing ->
      case rest of
        [] -> error $ "variable " ++ show n ++ " is not bound"
        next:rest' -> modifyVar n tup (prefix ++ [curr]) next rest'

insertVarT :: Ident -> (Type, Mutability) -> TC ()
insertVarT x tup = modify (\env -> 
  let scope  = topScope env
      scope' = M.insert x tup scope
  in  env { scopes = scope':(tail (scopes env)) })

  -- modify $ \s -> s { vars = M.insert x p (vars s) }

lookupVarT :: Ident -> TC (Type, Mutability)
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
