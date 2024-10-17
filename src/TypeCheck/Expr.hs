module TypeCheck.Expr where

import Evaluator

import Env

import Value ( TClosure( TFun ) )

import Lang.Abs ( Exp(..)
                , Ident
                , Type(..) )

arithmetic :: (Exp, Exp) -> (Env Type, Env TClosure) -> Result Type
arithmetic (e1, e2) env = do
    t1 <- infer e1 env
    t2 <- infer e2 env
    case (t1, t2) of
        (TInt, TInt) -> return TInt
        _            -> throw "Arithmetic can only be performed on integers"

logic :: (Exp, Exp) -> (Env Type, Env TClosure) -> Result Type
logic (e1, e2) env =  do
    t1 <- infer e1 env
    t2 <- infer e2 env
    case (t1, t2) of
        (TBool, TBool) -> return TBool
        _              -> throw "Boolean operations can only be performed on booleans"

comparison :: (Exp, Exp) -> (Env Type, Env TClosure) -> Result Type
comparison (e1, e2) env =  do
    t1 <- infer e1 env
    t2 <- infer e2 env
    case (t1, t2) of
        (TInt, TInt) -> return TBool
        (t1,   t2  ) -> throw $ "Cannot compare " ++ show t1 ++ " with " ++ show t2

-- EXPRESSION TYPE CHECKER -----------------------------------------------------------

infer :: Exp -> (Env Type, Env TClosure) -> Result Type

-- Arithmetic
infer (EInt _) _ = return TInt

infer (EMul e1 e2) env = arithmetic (e1, e2) env
infer (EDiv e1 e2) env = arithmetic (e1, e2) env
infer (EAdd e1 e2) env = arithmetic (e1, e2) env
infer (ESub e1 e2) env = arithmetic (e1, e2) env

-- Booleans
infer ETrue  _ = return TBool
infer EFalse _ = return TBool

infer (ENot e) env = do
    t <- infer e env
    case t of
        TBool -> return TBool
        _     -> throw "Boolean operations can only be performed on booleans"
infer (EAnd e1 e2) env = logic (e1, e2) env
infer (EOr  e1 e2) env = logic (e1, e2) env

-- Comparisons
infer (EEq e1 e2) env = do
    t1 <- infer e1 env
    t2 <- infer e2 env
    if t1 == t2 then return TBool
    else throw "Cannot compare different types"
infer (ELt  e1 e2) env = comparison (e1, e2) env
infer (EGt  e1 e2) env = comparison (e1, e2) env
infer (ELeq e1 e2) env = comparison (e1, e2) env
infer (EGeq e1 e2) env = comparison (e1, e2) env

-- Control flow
infer (EIf c iff els) env = do
    cond <- infer c env
    case cond of
        TBool -> do
            tI <- infer iff env
            tE <- infer els env
            case (tI, tE) of
                (tI, tE) | tI == tE -> return tI
                _                   -> throw "Both branches of an if must have the same type"
        _     -> throw "Condition must be a boolean"

-- Let bindings
infer (ELet x e body) env@(vars, funs) = do
    t <- infer e env
    infer body (bind x t vars, funs)
infer (EVar x) (vars, _) =
    case find x vars of
        Just t  -> return t
        Nothing -> throw $ "Variable " ++ show x ++ " is not bound"

-- Functions
infer (EApp f e) env@(_, funs) = do
    case find f funs of
        Just (TFun targ tret) -> do
            eT <- infer e env
            if eT == targ then return tret
            else throw "Function argument type mismatch"
        _        -> throw "Cannot call non-function"
