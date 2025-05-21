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

infer :: Program -> TC Type
infer (Program stmts exp) = do
    prepare stmts
    E.infer exp
  where
    prepare :: [Stmt] -> TC ()
    prepare []           = return ()
    prepare (stmt:stmts) = do
        S.infer stmt 
        prepare stmts 
