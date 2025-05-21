module TypeCheck.Expr where

import Evaluator

import Env

import Value ( TClosure( TFun )
             , Mutability( Imm, Mut ))

import Lang.Abs ( Exp(..)
                , Ident
                , Type(..) )

import qualified Data.Map.Strict as M
import Control.Monad
import Control.Monad.Trans.State.Strict


arithmetic :: (Exp, Exp) -> TC Type
arithmetic (e1, e2) = do
    t1 <- infer e1
    t2 <- infer e2
    case (t1, t2) of
        (TInt, TInt) -> return TInt
        _            -> throwError "Arithmetic can only be performed on integers"

logic :: (Exp, Exp) -> TC Type
logic (e1, e2) =  do
    t1 <- infer e1
    t2 <- infer e2
    case (t1, t2) of
        (TBool, TBool) -> return TBool
        _              -> throwError "Boolean operations can only be performed on booleans"

comparison :: (Exp, Exp) -> TC Type
comparison (e1, e2) =  do
    t1 <- infer e1
    t2 <- infer e2
    case (t1, t2) of
        (TInt, TInt) -> return TBool
        (t1,   t2  ) -> throwError $ "Cannot compare " ++ show t1 ++ " with " ++ show t2

-- EXPRESSION TYPE CHECKER -----------------------------------------------------------

infer :: Exp -> TC Type

infer EVoid = return TUnit

-- Arithmetic
infer (EInt _) = return TInt

infer (EMul e1 e2) = arithmetic (e1, e2)
infer (EDiv e1 e2) = arithmetic (e1, e2)
infer (EAdd e1 e2) = arithmetic (e1, e2)
infer (ESub e1 e2) = arithmetic (e1, e2)

-- Booleans
infer ETrue  = return TBool
infer EFalse = return TBool

infer (ENot e) = do
    t <- infer e 
    case t of
        TBool -> return TBool
        _     -> throwError "Boolean operations can only be performed on booleans"
infer (EAnd e1 e2) = logic (e1, e2) 
infer (EOr  e1 e2) = logic (e1, e2) 

-- Comparisons
infer (EEq e1 e2) = do
    t1 <- infer e1
    t2 <- infer e2
    if t1 == t2 then return TBool
    else throwError "Cannot compare different types"
infer (ELt  e1 e2) = comparison (e1, e2)
infer (EGt  e1 e2) = comparison (e1, e2)
infer (ELeq e1 e2) = comparison (e1, e2)
infer (EGeq e1 e2) = comparison (e1, e2)

-- Control flow
-- infer (EIfElse c iff els) = do
--     cond <- infer c 
--     case cond of
--         TBool -> do
--             tI <- infer iff 
--             tE <- infer els 
--             case (tI, tE) of
--                 (tI, tE) | tI == tE -> return tI
--                 _                   -> throwError "Both branches of an if must have the same type"
--         _     -> throwError "Condition must be a boolean"

-- Let bindings
-- infer (ELet x e body) = do
--     t <- infer e 
--     withBoundVarT x (t, Imm) (infer body)
infer (EVar x) = fst <$> lookupVarT x

-- Functions
infer (EApp f args) = do
    (TFun paramsT_Muts retTy, _) <- lookupFunT f
    -- done by both the typechecker and the interpreter
    when (length paramsT_Muts /= length args) $
       throwError $ "function " ++ show f ++ " expects " ++ show (length paramsT_Muts) ++ " arguments"
    
    forM_ (zip args paramsT_Muts) $ \(arg, expectedT) -> do
        e <- get
        actualT <- infer arg
        when (actualT /= fst expectedT) $ throwError $ 
            "type mismatch in call to " ++ show f ++
            ": expected " ++ show expectedT ++
            " but got " ++ show actualT

    return retTy

