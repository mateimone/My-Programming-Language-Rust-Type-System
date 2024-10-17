module Interp.Stmt where

import Evaluator

import Lang.Abs ( Stmt(..) )

import Env
import Value ( Value
             , Closure( Fun ) )

import qualified Interp.Expr as E

-- STATEMENT INTERPRETER -------------------------------------------------------------

interp :: Stmt -> (Env Value, Env Closure) -> Result (Env Value, Env Closure)

interp (SLet x e) env@(vars, funs) = do
    val <- E.interp e env
    return (bind x val vars, funs)

interp (SFun f x _ e) env@(vars, funs) = return (vars, bind f (Fun x e) funs)
