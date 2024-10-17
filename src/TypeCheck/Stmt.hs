module TypeCheck.Stmt where

import Evaluator

import Env

import Value ( TClosure( TFun ) )

import Lang.Abs ( Stmt(..)
                , Type )

import qualified TypeCheck.Expr as E

-- STATEMENT TYPE CHECKER ------------------------------------------------------------

infer :: Stmt -> (Env Type, Env TClosure) -> Result (Env Type, Env TClosure)

infer (SLet x e) env@(vars, funs) = do
    t <- E.infer e env
    return (bind x t vars, funs)

infer (SFun f x t e) (vars, funs) = do
    ret <- E.infer e (bind x t vars, funs)
    return (vars, bind f (TFun t ret) funs)
