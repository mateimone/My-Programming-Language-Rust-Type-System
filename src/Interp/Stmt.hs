module Interp.Stmt where

import Evaluator

import Lang.Abs ( Stmt(..), Arg(ArgImm, ArgMut), Ident )

import Env
import Value ( Value (..)
             , Closure( Fun )
             , Mutability( Imm, Mut ) )

import qualified Interp.Expr as E
import qualified Data.Map.Strict as M

import Data.Set as S
import Control.Monad
import Control.Monad.State.Strict

-- STATEMENT INTERPRETER -------------------------------------------------------------

-- type Result a = Either String a
-- type Env a = Map.Map Ident (a, Mutability)
interp :: Stmt -> Eval ()

-- Supports redeclaration of variables, be they mutable or immutable
interp (SLet x e) = do
    val <- E.interp e
    insertVar x (val, Imm)

interp (SLetAnn x ty e) = interp (SLet x e) 

interp (SLetM x e) = do
    val <- E.interp e
    insertVar x (val, Mut)

interp (SLetMAnn x ty e) = interp (SLetM x e)

interp (SFun f params retTy body lastE) = do
  let bindInfo = [ (n, if isMut then Mut else Imm)
                 | p <- params
                 , let (n,isMut) = case p of
                         ArgImm i _ -> (i,False)
                         ArgMut i _ -> (i,True) ]

  insertFun f (Fun bindInfo body lastE, Imm)

-- Currently cannot use functions as first class values
interp (SAss x e) = -- case x of 
    do
      v <- E.interp e
      assignVar x v

interp w@(SWhile cond stmts) = do
    e <- get
    -- liftIO $ print (scopes e)
    cont <- E.interp cond 
    case cont of
        VBool False -> return ()
        VBool True  -> withScope (interpAndPass stmts) >> interp w

interp (SIf cond stmts) = do
    cont <- E.interp cond
    case cont of
        VBool False -> return ()
        VBool True  -> withScope (interpAndPass stmts)

interp (SIfElse cond thn els) = do
    cont <- E.interp cond
    case cont of
        VBool False -> withScope (interpAndPass els)
        VBool True  -> withScope (interpAndPass thn)


-- HELPERS --

-- isMut :: Arg -> Bool
-- isMut (ArgMut _ _) = True
-- isMut (ArgImm _ _) = False

interpAndPass :: [Stmt] -> Eval ()
interpAndPass [] = return ()
interpAndPass stmts = do
    let stmt = head stmts
    interp stmt
    interpAndPass (tail stmts)
