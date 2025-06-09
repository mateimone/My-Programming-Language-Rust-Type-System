module TypeCheck.Prog where

import Evaluator

import Env

import Value ( TClosure, isCopy )

import Lang.Abs ( Program( Program )
                , Stmt
                , Type
                , Exp(..) )

import qualified TypeCheck.Stmt as S
import qualified TypeCheck.Expr as E
import qualified Lang.ErrM as S
import Control.Monad.State (gets)

-- PROGRAM TYPE CHECKER --------------------------------------------------------------

checkMoveNotAllowed :: Type -> Exp -> TC ()
checkMoveNotAllowed t e@(EIdx vec i) 
  | isCopy t = return ()
  | otherwise= throwError ("Cannot move element from array in expression " ++ show e)
checkMoveNotAllowed _ _ = return ()

infer :: Program -> TC Type
infer (Program stmts exp) = do
    prepare stmts
    eTy <- E.infer exp
    checkMoveNotAllowed eTy exp
    s <- gets scopes
    liftIO $ print s
    return eTy
  where
    prepare :: [Stmt] -> TC ()
    prepare []           = return ()
    prepare (stmt:stmts) = do
        S.infer stmt 
        prepare stmts 
