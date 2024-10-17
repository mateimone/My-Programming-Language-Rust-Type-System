module Interp.Prog where

import Evaluator

import Env
import Value ( Value
             , Closure )

import Lang.Abs ( Program( Program )
                , Stmt )

import qualified Interp.Stmt as S
import qualified Interp.Expr as E

-- PROGRAM INTERPRETER ---------------------------------------------------------------

interp :: Evaluator Value Closure
interp (Program stmts exp) env = do
    nenv <- prepare stmts env
    E.interp exp nenv
  where
    prepare :: [Stmt] -> (Env Value, Env Closure) -> Result (Env Value, Env Closure)
    prepare []           env = return env
    prepare (stmt:stmts) env = do
        nenv <- S.interp stmt env
        prepare stmts nenv
