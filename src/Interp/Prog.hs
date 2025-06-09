module Interp.Prog where

import Evaluator

import Env
import Value ( Value
             , Closure, isCopy )

import Lang.Abs ( Program( Program )
                , Stmt(..), Type, Exp(..) )

import qualified Interp.Stmt as S
import qualified Interp.Expr as E
import Control.Monad.State (gets)

-- PROGRAM INTERPRETER ---------------------------------------------------------------

interp :: Program -> Eval Value
interp (Program stmts exp) = do
    prepare stmts
    e <- E.interp exp
    s <- gets scopes
    rs <- gets refStore
    liftIO $ print s
    liftIO $ print rs
    return e
  where
    prepare :: [Stmt] -> Eval ()
    prepare []           = return ()
    prepare (stmt:stmts) = do
        nenv <- S.interp stmt
        prepare stmts 

