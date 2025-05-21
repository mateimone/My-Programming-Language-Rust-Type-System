module Interp.Expr where

import Evaluator

import Env hiding (gets, modify)
import Value
import Lang.Abs ( Exp(..), Ident, Stmt )
import qualified Data.Map.Strict as M
import Control.Monad 
import Control.Monad.Trans.State.Strict
import {-# SOURCE #-} Interp.Stmt as S
-- import qualified Interp.Stmt as S

arithmetic :: (Exp, Exp) -> (Integer -> Integer -> Integer) -> Eval Value
arithmetic (e1, e2) f = do
    v1 <- Interp.Expr.interp e1
    v2 <- Interp.Expr.interp e2
    case (v1, v2) of
        (VInt i1, VInt i2) -> return $ VInt (f i1 i2)
        _                  -> throwError "Arithmetic can only be performed on integers"

logic :: (Exp, Exp) -> (Bool -> Bool -> Bool) -> Eval Value
logic (e1, e2) f =  do
    v1 <- Interp.Expr.interp e1
    v2 <- Interp.Expr.interp e2
    case (v1, v2) of
        (VBool b1, VBool b2) -> return $ VBool (f b1 b2)
        _                    -> throwError "Boolean operations can only be performed on booleans"
        
-- EXPRESSION INTERPRETER ------------------------------------------------------------
interp :: Exp -> Eval Value

interp EVoid = return VUnit

-- Arithmetic
interp (EInt i) = return $ VInt i

interp (EMul e1 e2) = arithmetic (e1, e2) (*)
interp (EDiv e1 e2) = arithmetic (e1, e2) div
interp (EAdd e1 e2) = arithmetic (e1, e2) (+)
interp (ESub e1 e2) = arithmetic (e1, e2) (-) 

-- Booleans
interp ETrue  = return $ VBool True
interp EFalse = return $ VBool False

interp (ENot e) =  do
    v <- Interp.Expr.interp e
    case v of
        VBool b -> return $ VBool (not b)
        _       -> throwError "Boolean operations can only be performed on booleans"
interp (EAnd e1 e2) = logic (e1, e2) (&&)
interp (EOr e1 e2)  = logic (e1, e2) (||)

-- Comparisons
interp (EEq e1 e2) = do
    v1 <- Interp.Expr.interp e1
    v2 <- Interp.Expr.interp e2
    case (v1, v2) of
        (VBool b1, VBool b2) -> return $ VBool (b1 == b2)
        (VInt  i1, VInt  i2) -> return $ VBool (i1 == i2)
        _                    -> throwError "Cannot compare different types"
interp (ELt e1 e2) = do
    v1 <- Interp.Expr.interp e1
    v2 <- Interp.Expr.interp e2
    case (v1, v2) of
        (VInt i1, VInt i2) -> return $ VBool (i1 < i2)
        _                  -> throwError "Cannot compare different types"
interp (EGt e1 e2) = do
    v1 <- Interp.Expr.interp e1
    v2 <- Interp.Expr.interp e2
    case (v1, v2) of
        (VInt i1, VInt i2) -> return $ VBool (i1 > i2)
        _                  -> throwError "Cannot compare different types"
interp (ELeq e1 e2) = do
    v1 <- Interp.Expr.interp e1 
    v2 <- Interp.Expr.interp e2 
    case (v1, v2) of
        (VInt i1, VInt i2) -> return $ VBool (i1 <= i2)
        _                  -> throwError "Cannot compare different types"
interp (EGeq e1 e2) = do
    v1 <- Interp.Expr.interp e1 
    v2 <- Interp.Expr.interp e2 
    case (v1, v2) of
        (VInt i1, VInt i2) -> return $ VBool (i1 >= i2)
        _                  -> throwError "Cannot compare different types"

-- Control flow
-- interp (EIfElse c iff els) = do
--     cond <- interp c 
--     case cond of
--         VBool True  -> interp iff
--         VBool False -> interp els
--         _           -> throwError "Condition must be a boolean"

-- Let bindings
-- interp (ELet x e body) = do
--     arg <- Interp.Expr.interp e
--     withBoundVar x (arg, Imm) (Interp.Expr.interp body)
interp (EVar x) = fst <$> lookupVar x

-- Functions
interp (EApp f args) = do
    (Fun paramInfo stmts retE, _) <- lookupFun f
    
    when (length paramInfo /= length args) $
       throwError $ "function " ++ show f ++ " expects " ++ show (length paramInfo) ++ " arguments"
    
    argvs <- mapM Interp.Expr.interp args
    let funScope = M.fromList [ (name,(val,mut)) | ((name,mut),val) <- zip paramInfo argvs]
    outerEnv <- get
    put outerEnv { scopes = [funScope] }
    result <- do
              interpAndPass stmts
              Interp.Expr.interp retE
    put outerEnv
    return result

interpAndPass :: [Stmt] -> Eval ()
interpAndPass [] = return ()
interpAndPass stmts = do
    let stmt = head stmts
    S.interp stmt
    interpAndPass (tail stmts)
