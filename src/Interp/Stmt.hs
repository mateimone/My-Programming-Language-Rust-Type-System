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

-- STATEMENT INTERPRETER -------------------------------------------------------------

-- type Result a = Either String a
-- type Env a = Map.Map Ident (a, Mutability)
interp :: Stmt -> Eval ()

interp (SExp e) = do
    E.interp e
    return ()

-- Supports redeclaration of variables, be they mutable or immutable
interp (SLet x e) = do
    val <- E.interp e
    insertVar x (Prim val, Imm)

interp (SLetAnn x ty e) = interp (SLet x e) 

interp (SLetM x e) = do
    val <- E.interp e
    insertVar x (Prim val, Mut)

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
      assignVar x (Prim v)

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

interp (SPush vec el) = do
    (VList addr) <- E.interp vec
    h <- use heapL
    let (Just (OList list)) = M.lookup addr h
    el' <- E.interp el
    let newSlot = Prim el'
    let newList = list ++ [newSlot]
    replaceObject addr (OList newList) 

interp (SInsert vec i el) = do
    (VList addr) <- E.interp vec
    h <- use heapL
    let (Just (OList list)) = M.lookup addr h
    el' <- E.interp el
    let newSlot = Prim el'
    (VInt idx) <- E.interp i
    -- let idx = fromInteger idx
    when (idx > (toInteger $ length list)) $ throwError "Insert: index to insert element larger than length of list"
    when (idx < 0) $ throwError "Insert: index to insert element is smaller than 0"
    let newList = insertAt newSlot idx list
    replaceObject addr (OList newList)

interp (SSetIdx vec idxs e) = do
    v <- E.interp vec
    (VList addr) <- case v of
                      s@(VList _) -> return s
                      t@(VMutRef a) -> maxUnwrapBorrowedValue t
    h <- use heapL
    let (Just (OList list)) = M.lookup addr h
    liftIO $ print addr
    newVal <- E.interp e
    let newSlot = Prim newVal 
    let exps = [e | IndexList e <- idxs]
    itXs <- mapM E.interp exps
    unwrapList list itXs newSlot addr

interp (SAssDeref exp v) = do 
    (VMutRef a) <- E.interp exp
    newVal <- E.interp v
    -- v <- getBorrowedValue a
    modifyBorrowedValue a (Prim newVal) 

unwrapList :: [Slot Value] -> [Value] -> Slot Value -> Addr -> Eval ()
unwrapList ls [(VInt i)] elemToSet currentAddr = do
    when (fromInteger i >= length ls) $ throwError $ "Index " ++ show i ++ " larger than the length of the list"
    let valAtIdx = ls !! (fromInteger i)
    h <- use heapL
    case valAtIdx of
        (Prim v) -> do
            let newLs = (Prelude.take (fromInteger i) ls) ++ [elemToSet] ++ (Prelude.drop ((fromInteger i)+1) ls)
            heapL %= M.insert currentAddr (OList newLs)
            h' <- use heapL
            liftIO $ print h'
unwrapList ls ((VInt i):idxs) elemToSet currentAddr = do
    let valAtIdx = ls !! (fromInteger i)
    h <- use heapL
    liftIO $ print valAtIdx
    liftIO $ print ((VInt i):idxs)
    case valAtIdx of
        Prim (VList addr) -> do
            let (Just (OList nextList)) = M.lookup addr h
            unwrapList nextList idxs elemToSet addr
        Prim v -> do
            throwError "Trying to index non-array"


-- interp (SSetIdx vec i e) = do
--     -- (Prim (VList addr), Mut) <- lookupVar v
--     (VList addr) <- E.interp vec
--     h <- gets heap
--     let (Just (OList list)) = M.lookup addr h
--     newVal <- E.interp e
--     let newSlot = Prim newVal
--     (VInt idx) <- E.interp i
--     when (idx >= (toInteger $ length list)) $ throwError "Set: index to set element at larger than length of list"
--     when (idx < 0) $ throwError "Set: index to set element at is smaller than 0"
--     let newList = modifyAt newSlot idx list
--     replaceObject addr (OList newList)

modifyAt :: a -> Integer -> [a] -> [a]
modifyAt newElement 0 (a:as) = newElement:as
modifyAt newElement n (a:as) = a : modifyAt newElement (n - 1) as

insertAt :: a -> Integer -> [a] -> [a]
insertAt newElement 0 as = newElement:as
insertAt newElement i (a:as) = a : insertAt newElement (i - 1) as

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
