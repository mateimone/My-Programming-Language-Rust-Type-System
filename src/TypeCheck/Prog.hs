module TypeCheck.Prog where

import Evaluator

import Env

import Value ( TClosure )

import Lang.Abs ( Program( Program )
                , Stmt
                , Type )

import qualified TypeCheck.Stmt as S
import qualified TypeCheck.Expr as E
import qualified Lang.ErrM as S

-- PROGRAM TYPE CHECKER --------------------------------------------------------------

infer :: Evaluator Type TClosure
infer (Program stmts exp) env = do
    nenv <- prepare stmts env
    E.infer exp nenv
  where
    prepare :: [Stmt] -> (Env Type, Env TClosure) -> Result (Env Type, Env TClosure)
    prepare []           env = return env
    prepare (stmt:stmts) env = do
        nenv <- S.infer stmt env
        prepare stmts nenv
