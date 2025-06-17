module Interp.Stmt where

import Evaluator

import Lang.Abs ( Stmt(..), Arg(ArgImm, ArgMut), Ident, Exp(..) , IndexList(..))

import Env
import Value ( Value (..)
             , Closure( Fun )
             , Mutability( Imm, Mut )
             , Object(..), Addr (..)
            )

import qualified Interp.Expr as E
import qualified Data.Map.Strict as M

import Data.Set as S
import Control.Monad
import Control.Monad.State.Strict
import Control.Lens
import FiniteMap

-- STATEMENT INTERPRETER -------------------------------------------------------------
interp :: Stmt -> Eval ()

-- SExp - interpret an expression and discard its value
interp (SExp e) = do
    E.interp e
    return ()

-- Supports redeclaration of variables, be they mutable or immutable
-- SLet: create a variable, give a value to it
interp (SLet x e) = do
    val <- E.interp e
    insertVar x (val, Imm)

-- SLetAnn: create a variable with an explicit type annotation, give a value to it
interp (SLetAnn x ty e) = interp (SLet x e) 

-- SLetM - create a mutable variable, give a value to it
interp (SLetM x e) = do
    val <- E.interp e
    insertVar x (val, Mut)

-- SLetMAnn - create a mutable variable with an explicit type annotation, give a value to it
interp (SLetMAnn x ty e) = interp (SLetM x e)

-- SFun - function definition. We insert the function in the function store
interp (SFun f params retTy body lastE) = do
  let bindInfo = [ (n, if isMut then Mut else Imm)
                 | p <- params
                 , let (n,isMut) = case p of
                         ArgImm i _ -> (i,False)
                         ArgMut i _ -> (i,True) ]

  insertFun f (Fun bindInfo body lastE)

-- SAss - assign a value to a variable
interp (SAss x e) =
    do
      v <- E.interp e
      assignVar x v

-- SArtBlock - artificial "{}" block: any new bindings are dropped at the end of this block
interp (SArtBlock stmts) = do
    withScope (interpAndPass stmts)

-- SWhile: while loop, any new bindings are dropped at the endv
interp w@(SWhile cond stmts) = do
    e <- get
    cont <- E.interp cond 
    case cont of
        VBool False -> return ()
        VBool True  -> withScope (interpAndPass stmts) >> interp w

-- SIf: if statement, any new bindings are dropped at the end
interp (SIf cond stmts) = do
    cont <- E.interp cond
    case cont of
        VBool False -> return ()
        VBool True  -> withScope (interpAndPass stmts)

-- SIfElse: if-else statement, any new bindings are dropped at the end; 
-- the environments of the two branches are separate
interp (SIfElse cond thn els) = do
    cont <- E.interp cond
    case cont of
        VBool False -> withScope (interpAndPass els)
        VBool True  -> withScope (interpAndPass thn)

-- Push a value into a vector
interp (SPush vec el) = do
    (VList addr) <- E.interp vec
    h <- use heapL
    let (Just (OList list)) = lookupFM addr h
    el' <- E.interp el
    let newSlot = el'
    let newList = list ++ [newSlot]
    replaceObject addr (OList newList) 

-- Insert a value at a position in a vector
interp (SInsert vec i el) = do
    (VList addr) <- E.interp vec
    h <- use heapL
    let (Just (OList list)) = lookupFM addr h
    el' <- E.interp el
    let newSlot = el'
    (VInt idx) <- E.interp i
    when (idx > (toInteger $ length list)) $ throwError "Insert: index to insert element larger than length of list"
    when (idx < 0) $ throwError "Insert: index to insert element is smaller than 0"
    let newList = insertAt newSlot idx list
    replaceObject addr (OList newList)

-- Set the value at a position in a vector
interp (SSetIdx vec idxs e) = do
    v <- E.interp vec
    (VList addr) <- case v of
                      s@(VList _) -> return s
                      t@(VMutRef a) -> maxUnwrapBorrowedValue t
    h <- use heapL
    let (Just (OList list)) = lookupFM addr h
    newVal <- E.interp e
    let newSlot = newVal 
    let exps = [e | IndexList e <- idxs]
    itXs <- mapM E.interp exps
    unwrapList list itXs newSlot addr

-- Mutate the value under a mutable reference
interp (SAssDeref exp v) = do 
    (VMutRef a) <- E.interp exp
    newVal <- E.interp v
    modifyBorrowedValue a newVal

-- Sets the value at a (possibly nested) position in a (possibly nested) vector
unwrapList :: [Value] -> [Value] -> Value -> Addr -> Eval ()
unwrapList ls [(VInt i)] elemToSet currentAddr = do
    when (fromInteger i >= length ls) $ throwError $ "Index " ++ show i ++ " larger than the length of the list"
    let valAtIdx = ls !! (fromInteger i)
    h <- use heapL
    case valAtIdx of
        v -> do
            let newLs = (Prelude.take (fromInteger i) ls) ++ [elemToSet] ++ (Prelude.drop ((fromInteger i)+1) ls)
            heapL %= bind currentAddr (OList newLs)
unwrapList ls ((VInt i):idxs) elemToSet currentAddr = do
    let valAtIdx = ls !! (fromInteger i)
    h <- use heapL
    case valAtIdx of
        (VList addr) -> do
            let (Just (OList nextList)) = lookupFM addr h
            unwrapList nextList idxs elemToSet addr
        v -> do
            throwError "Trying to index non-array"

-- Modify an element at a position in a list
modifyAt :: a -> Integer -> [a] -> [a]
modifyAt newElement 0 (a:as) = newElement:as
modifyAt newElement n (a:as) = a : modifyAt newElement (n - 1) as

-- Insert an element at a position in a list
insertAt :: a -> Integer -> [a] -> [a]
insertAt newElement 0 as = newElement:as
insertAt newElement i (a:as) = a : insertAt newElement (i - 1) as

-- Sequentially interpret a list of statements
interpAndPass :: [Stmt] -> Eval ()
interpAndPass [] = return ()
interpAndPass stmts = do
    let stmt = head stmts
    interp stmt
    interpAndPass (tail stmts)
