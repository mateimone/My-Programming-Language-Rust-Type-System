module TypeCheck.Stmt where

import Evaluator

import Env

import Value ( TClosure( TFun )
             , Mutability( Imm, Mut ) )

import Lang.Abs ( Stmt(..)
                , Type(..)
                , Ident
                , Arg(..)
                , Exp(..) )

import qualified TypeCheck.Expr as E
import qualified Data.Map.Strict as M
import Data.List (foldl')
import Control.Monad
import Control.Monad.Trans.State.Strict

-- STATEMENT TYPE CHECKER ------------------------------------------------------------

voidNotAllowed :: Type -> Exp -> Stmt -> TC ()
voidNotAllowed t e st = when (t == TUnit) $ throwError ("Type cannot be unit in expression: " ++ show e ++ "\nIn statement: " ++ show st)

infer :: Stmt -> TC ()

infer s@(SLet x e) = do
    t <- E.infer e
    voidNotAllowed t e s
    insertVarT x (t, Imm)

infer s@(SLetAnn x ty e) = do
  t <- E.infer e
  voidNotAllowed t e s
  if t == ty
     then insertVarT x (t, Imm) -- >> do {e <- get; (liftIO $ print (scopes e))}
     else throwError $
       "Type mismatch for " ++ show x ++
       ": annotation says " ++ show ty ++
       " but expression has type " ++ show t

infer s@(SLetM x e) = do
    t <- E.infer e
    voidNotAllowed t e s
    insertVarT x (t, Mut)

infer s@(SLetMAnn x ty e) = do
  t <- E.infer e 
  voidNotAllowed t e s
  if t == ty
     then insertVarT x (t, Mut)
     else throwError $
       "Type mismatch for " ++ show x ++
       ": annotation says " ++ show ty ++
       " but expression has type " ++ show t

-- infer (SFunNp f retTy stmts lastE) = do

infer (SFun f params retTy stmts lastE) = do  
  let bindInfo = [ (name, (t, if isMut then Mut else Imm))
               | p <- params
               , let (name, t, isMut) = case p of
                       ArgImm nm ty -> (nm , ty , False)
                       ArgMut nm ty -> (nm , ty , True) ]

  let argst = map (\(_,(t,_)) -> t) bindInfo
  let argMuts = map (\(_,(_,m)) -> m) bindInfo
  let paramScope = M.fromList bindInfo
  outerEnv <- get
  put outerEnv { scopes = [paramScope] }

  tBody <- do
              inferAndPass stmts
              E.infer lastE

  put outerEnv
  
  when (tBody /= retTy) $ 
      throwError $ "return type mismatch in " ++ show f ++
        ": annotation says " ++ show retTy ++
        ", but body returns " ++ show tBody
  insertFunT f (TFun (zip argst argMuts) retTy, Imm)
  

-- Currently cannot use functions as first class values
infer s@(SAss x e) = do
  (t, mut) <- lookupVarT x
  case mut of
    Imm -> throwError ("cannot assign to immutable " ++ show x)
    Mut -> do
      ty <- E.infer e
      voidNotAllowed ty e s
      if t == ty then return ()
      else throwError $ "Type mismatch in assignment to " ++ show x
                      ++ ": variable has type " ++ show t
                      ++ ", but expression has type " ++ show ty

infer w@(SWhile cond stmts) = do
  e <- get
  -- liftIO $ print (scopes e)
  tCond <- E.infer cond
  case tCond of
    TBool -> withScopeT (inferAndPass stmts)
    _     -> throwError $ "Condition does not have bool type in while statement " ++ show w

infer i@(SIf cond stmts) = do
  cont <- E.infer cond
  case cont of
    TBool -> withScopeT (inferAndPass stmts)
    _     -> throwError $ "Condition does not have bool type in if statement " ++ show i

infer i@(SIfElse cond thn els) = do
    cont <- E.infer cond
    case cont of
      TBool -> withScopeT (inferAndPass thn) >> withScopeT (inferAndPass els)
      _     -> throwError $ "Condition does not have bool type in if statement " ++ show i

inferAndPass :: [Stmt] -> TC ()
inferAndPass [] = return ()
inferAndPass stmts = do
    let stmt = head stmts
    infer stmt
    inferAndPass (tail stmts)
