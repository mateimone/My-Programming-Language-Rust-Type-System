module Interp.Expr where

import Evaluator

import Env hiding (gets, modify)
import Value
import Lang.Abs ( Exp(..), Ident(..), Stmt )
import qualified Data.Map.Strict as M
import Control.Monad
import Control.Monad.Trans.State.Strict
import {-# SOURCE #-} Interp.Stmt as S
import Data.List.Extra
import System.Random (mkStdGen)
import Control.Lens
import FiniteMap


arithmetic :: (Exp, Exp) -> (Integer -> Integer -> Integer) -> Eval Value
arithmetic (e1, e2) f = do
    v1 <- Interp.Expr.interp e1
    v2 <- Interp.Expr.interp e2
    case (v1, v2) of
        (VInt i1, VInt i2) -> return $ VInt (f i1 i2)
        (VRef a, VInt i2) -> do 
            (VInt i1) <- getBorrowedValue a
            return $ VInt (f i1 i2)
        (VInt i1, VRef a) -> do 
            (VInt i2) <- getBorrowedValue a
            return $ VInt (f i1 i2)
        (VRef a, VRef b) -> do
            (VInt i1) <- getBorrowedValue a
            (VInt i2) <- getBorrowedValue b
            return $ VInt (f i1 i2)
        _                  -> throwError "Arithmetic can only be performed on integers"

logic :: (Exp, Exp) -> (Bool -> Bool -> Bool) -> Eval Value
logic (e1, e2) f =  do
    v1 <- Interp.Expr.interp e1
    v2 <- Interp.Expr.interp e2
    case (v1, v2) of
        (VBool b1, VBool b2) -> return $ VBool (f b1 b2)
        _                    -> throwError "Boolean operations can only be performed on booleans"

-- EXPRESSION INTERPRETER ------------------------------------------------------------
interp :: Exp -> Eval Value

interp EVoid = return VUnit

-- Arithmetic
interp (EInt i) = return $ VInt i

interp (EMul e1 e2) = arithmetic (e1, e2) (*)
interp (EDiv e1 e2) = arithmetic (e1, e2) div
interp (EAdd e1 e2) = arithmetic (e1, e2) (+)
interp (ESub e1 e2) = arithmetic (e1, e2) (-)

-- Booleans
interp ETrue  = return $ VBool True
interp EFalse = return $ VBool False

interp (ENot e) =  do
    v <- Interp.Expr.interp e
    case v of
        VBool b -> return $ VBool (not b)
        VRef a -> do
            (VBool b) <- getBorrowedValue a
            return $ VBool (not b)
        _       -> throwError "Boolean operations can only be performed on booleans"
interp (EAnd e1 e2) = logic (e1, e2) (&&)
interp (EOr e1 e2)  = logic (e1, e2) (||)

-- Comparisons
interp (EEq e1 e2) = do
    v1 <- Interp.Expr.interp e1
    v2 <- Interp.Expr.interp e2
    case (v1, v2) of
        (VBool b1, VBool b2) -> return $ VBool (b1 == b2)
        (VInt  i1, VInt  i2) -> return $ VBool (i1 == i2)
        (VLight c1, VLight c2) -> return $ VBool (c1 == c2)
        (VList l1, VList l2) -> return $ VBool (l1 == l2)
        (VRef a, VRef b) -> do
            vr1 <- maxUnwrapBorrowedValue (VRef a)
            vr2 <- maxUnwrapBorrowedValue (VRef b)
            return $ VBool (vr1 == vr2)
        (VMutRef a, VMutRef b) -> do
            vr1 <- maxUnwrapBorrowedValue (VMutRef a)
            vr2 <- maxUnwrapBorrowedValue (VMutRef b)
            return $ VBool (vr1 == vr2)
        (VMutRef a, VRef b) -> do
            vr1 <- maxUnwrapBorrowedValue (VMutRef a)
            vr2 <- maxUnwrapBorrowedValue (VRef b)
            return $ VBool (vr1 == vr2)
        (VRef a, VMutRef b) -> do
            vr1 <- maxUnwrapBorrowedValue (VRef a)
            vr2 <- maxUnwrapBorrowedValue (VMutRef b)
            return $ VBool (vr1 == vr2)
        _                    -> throwError "Cannot compare different types"
interp (ELt e1 e2) = do
    v1 <- Interp.Expr.interp e1
    v2 <- Interp.Expr.interp e2
    case (v1, v2) of
        (VInt i1, VInt i2) -> return $ VBool (i1 < i2)
        (VRef a, VRef b) -> do
            (VInt v1) <- maxUnwrapBorrowedValue (VRef a)
            (VInt v2) <- maxUnwrapBorrowedValue (VRef b)
            return $ VBool (v1 < v2)
        (VMutRef a, VMutRef b) -> do
            (VInt v1) <- maxUnwrapBorrowedValue (VMutRef a)
            (VInt v2) <- maxUnwrapBorrowedValue (VMutRef b)
            return $ VBool (v1 < v2)
        _                  -> throwError "Cannot compare different types"
interp (EGt e1 e2) = do
    v1 <- Interp.Expr.interp e1
    v2 <- Interp.Expr.interp e2
    case (v1, v2) of
        (VInt i1, VInt i2) -> return $ VBool (i1 > i2)
        (VRef a, VRef b) -> do
            (VInt v1) <- maxUnwrapBorrowedValue (VRef a)
            (VInt v2) <- maxUnwrapBorrowedValue (VRef b)
            return $ VBool (v1 > v2)
        (VMutRef a, VMutRef b) -> do
            (VInt v1) <- maxUnwrapBorrowedValue (VMutRef a)
            (VInt v2) <- maxUnwrapBorrowedValue (VMutRef b)
            return $ VBool (v1 > v2)
        _                  -> throwError "Cannot compare different types"
interp (ELeq e1 e2) = do
    v1 <- Interp.Expr.interp e1
    v2 <- Interp.Expr.interp e2
    case (v1, v2) of
        (VInt i1, VInt i2) -> return $ VBool (i1 <= i2)
        (VRef a, VRef b) -> do
            (VInt v1) <- maxUnwrapBorrowedValue (VRef a)
            (VInt v2) <- maxUnwrapBorrowedValue (VRef b)
            return $ VBool (v1 <= v2)
        (VMutRef a, VMutRef b) -> do
            (VInt v1) <- maxUnwrapBorrowedValue (VMutRef a)
            (VInt v2) <- maxUnwrapBorrowedValue (VMutRef b)
            return $ VBool (v1 <= v2)
        _                  -> throwError "Cannot compare different types"
interp (EGeq e1 e2) = do
    v1 <- Interp.Expr.interp e1
    v2 <- Interp.Expr.interp e2
    case (v1, v2) of
        (VInt i1, VInt i2) -> return $ VBool (i1 >= i2)
        (VRef a, VRef b) -> do
            (VInt v1) <- maxUnwrapBorrowedValue (VRef a)
            (VInt v2) <- maxUnwrapBorrowedValue (VRef b)
            return $ VBool (v1 >= v2)
        (VMutRef a, VMutRef b) -> do
            (VInt v1) <- maxUnwrapBorrowedValue (VMutRef a)
            (VInt v2) <- maxUnwrapBorrowedValue (VMutRef b)
            return $ VBool (v1 >= v2)
        _                  -> throwError "Cannot compare different types"

-- EVar - Variable lookup and move if not non-copy
interp (EVar x) = fst <$> readPrim x

-- ELight - light literal; can take 3 values: Red, Yellow, Green
interp (ELight color) = return (VLight color)

-- EVec - list literal; vec![...]
interp (EVec es) = do
    eis <- mapM Interp.Expr.interp es
    addr <- insertInHeap (OList eis)
    h <- use heapL
    return (VList addr)

-- Index a variable
interp (EIdx vec i) = do
    o <- Interp.Expr.interp vec
    (VList addr) <- case o of
                        (VList a) -> return (VList a)
                        r@(VRef a) -> maxUnwrapBorrowedValue r
                        r@(VMutRef a) -> maxUnwrapBorrowedValue r
    list@(OList elems) <- readObject addr
    (VInt idx) <- Interp.Expr.interp i
    if (length elems) <= fromInteger idx then 
        throwError $ "Index " ++ show i ++ " is out of bounds for " ++ show list 
    else (let el = elems !! (fromInteger idx)
        in  case el of
            i@(VInt _) -> return i
            b@(VBool _) -> return b
            u@VUnit -> return u
            x -> return x
        )

-- ERef - immutable borrow
interp (ERef exp) = do
    e <- Interp.Expr.interp exp
    case exp of
        (EVar var) -> do
            a@(Addr refAddr) <- insertInStore (var, e, Imm)
            return (VRef a)
        otherVal -> do
            let (tempVarString, g) = randomString (mkStdGen 42)
            a@(Addr refAddr) <- insertInStore (Ident tempVarString, e, Imm) 
            return (VRef a)

-- EMutRef - mutable borrow
interp (EMutRef exp) = do
    e <- Interp.Expr.interp exp

    case exp of
        (EVar var) -> do
            a@(Addr refAddr) <- insertInStore (var, e, Mut)
            return (VMutRef a)
        otherVal -> do
            let (tempVarString, g) = randomString (mkStdGen 42)
            a@(Addr refAddr) <- insertInStore (Ident tempVarString, e, Mut)
            return (VMutRef a)

-- EDeref - access the value under a reference
interp (EDeref exp) = do
    e <- Interp.Expr.interp exp
    case e of
        (VRef addr) -> do
            val <- getBorrowedValue addr
            return val
        (VMutRef addr) -> do
            val <- getBorrowedValue addr
            return val
        -- _ -> throwError "Can only dereference a reference"

-- ERemove - remove the element at a position in a list
interp (ERemove vec i) = do
    (VList addr) <- Interp.Expr.interp vec
    h <- use heapL
    let (Just (OList list)) = lookupFM addr h
    (VInt idx) <- Interp.Expr.interp i
    when (idx >= (fromIntegral $ length list)) (throwError $ "Index " ++ show idx ++ " out of bounds for array " ++ show list)
    let (e, rest) = removeElementAt idx list
    replaceObject addr (OList rest)
    case e of
        val -> return val

-- Function application
-- Get function from function store, check arity and number of arguments passed,
-- interpret arguments, discard whole previous environment for a single scope during execution
-- finally, place old environment back, while updating any mutable references that had 
-- their value changed
interp (EApp f args) = do
    (Fun paramInfo stmts retE) <- lookupFun f

    argvs <- mapM Interp.Expr.interp args
    let funScope = M.fromList [ (name,(val,mut)) | ((name,mut),val) <- zip paramInfo argvs]
    outerEnv <- get
    scopesL .= [funScope]
    refStoreL .= (M.empty:(refStore outerEnv))

    result <- do
              interpAndPass stmts
              Interp.Expr.interp retE
    store <- use refStoreL
    h <- use heapL
    nextadr <- use nextAL

    newSt <- case store of
                    top : rest@(fst:t) -> do
                        let newStore = (M.union top fst):t
                        put outerEnv { refStore = newStore, heap = h, nextA = nextadr } >> return newStore
                
                    [_] -> put outerEnv { refStore = store, heap = h, nextA = nextadr } >> return store
    
    -- put outerEnv { refStore = newStore, heap = h, nextA = nextadr }
    
    modifyEnvWithStoreAfterFN newSt

    return result

-- This function takes a list of references and updates the variable environment
-- shall any variable that had a mutable reference had the value under the reference changed
modifyEnvWithStoreAfterFN :: [M.Map Addr (Ident, Value, Mutability)] -> Eval ()
modifyEnvWithStoreAfterFN store = do
    mapM_ modifyEnvWithAStore store

-- This function updates a single variable that had a mutable reference for which
-- the value under the reference was changed
modifyEnvWithAStore :: M.Map Addr (Ident, Value, Mutability) -> Eval ()
modifyEnvWithAStore store = do
    mapM_ update (M.toList store)

-- If the referenced variable doesn't exist, ignore it, it was a reference to a value, not a variable
-- If the referenced variable does exist, update its value with the one in the reference store
update :: (Addr, (Ident, Value, Mutability)) -> Eval ()
update (addr, tup@(_, _, Imm)) = return ()
update (addr, tup@(name, newVal, Mut)) = do
    found <- lookupVarMaybe name
    s <- use refStoreL
    scs <- use scopesL
    case found of
        Nothing -> return ()
        Just (oldVal, Mut) 
            | oldVal == newVal -> return ()
            | otherwise -> assignVar name newVal

-- Insert an element at a position in a list
insertAt :: a -> Integer -> [a] -> [a]
insertAt newElement 0 as = newElement:as
insertAt newElement i (a:as) = a : insertAt newElement (i - 1) as

-- This function is called ONLY if the index is smaller than the length of the list
-- An error is thrown beforehand otherwise
removeElementAt :: Integer -> [a] -> (a, [a])
removeElementAt 0 (a:as) = (a, as)
removeElementAt n (a:as) = 
    let (x, xs) = removeElementAt (n-1) as
    in (x, (a:(dropEnd (length as + 1) xs)) ++ xs)

-- Sequentially interpret a list of statements
interpAndPass :: [Stmt] -> Eval ()
interpAndPass [] = return ()
interpAndPass stmts = do
    let stmt = head stmts
    S.interp stmt
    interpAndPass (tail stmts)
