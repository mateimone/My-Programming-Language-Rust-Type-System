module TypeCheck.Expr where

import Evaluator

import Env hiding (modify)

import Value ( TClosure( TFun )
             , Mutability( Imm, Mut )
             , VarInfo(..)
             , isCopy
             , Addr(..)
             , fitsInto )

import Lang.Abs ( Exp(..)
                , Ident
                , Type(..) )

import qualified Data.Map.Strict as M
import Control.Monad
import Control.Monad.Trans.State.Strict
import Control.Lens


checkMoveNotAllowed :: Type -> Exp -> TC ()
checkMoveNotAllowed t e@(EIdx vec i) 
  | isCopy t = return ()
  | otherwise= throwError ("Cannot move element from array in expression " ++ show e)
checkMoveNotAllowed _ _ = return ()

checkMultiple :: [Type] -> [Exp] -> TC ()
checkMultiple [] [] = return ()
checkMultiple (t:ts) (e:es) = checkMoveNotAllowed t e >> checkMultiple ts es

arithmetic :: (Exp, Exp) -> TC Type
arithmetic (e1, e2) = do
    t1 <- infer e1
    t2 <- infer e2
    case (t1, t2) of
        (TInt, TInt) -> return TInt
        (TRef TInt, TInt) -> return TInt
        (TInt, TRef TInt) -> return TInt
        (TRef TInt, TRef TInt) -> return TInt
        _            -> throwError "Arithmetic can only be performed on integers"

logic :: (Exp, Exp) -> TC Type
logic (e1, e2) =  do
    t1 <- infer e1
    t2 <- infer e2
    case (t1, t2) of
        (TBool, TBool) -> return TBool
        -- (TRef TBool, TBool) -> return TBool
        -- (TBool, TRef TBool) -> return TBool
        -- (TRef TBool, TRef TBool) -> return TBool
        _              -> throwError "Boolean operations can only be performed on booleans"

comparison :: (Exp, Exp) -> TC Type
comparison (e1, e2) =  do
    t1 <- infer e1
    t2 <- infer e2
    case (t1, t2) of
        (TInt, TInt) -> return TBool
        -- (TRef TInt, TInt) -> return TBool
        -- (TInt, TRef TInt) -> return TBool
        (TRef t1, TRef t2) -> do
            let (u1, _) = numberOfRefs t1
            let (u2, _) = numberOfRefs t2
            if t1 == t2 && (u1 == TInt) && (u2 == TInt) then return TBool else throwError $ "Cannot compare " ++ show t1 ++ " with " ++ show t2
        (TMutRef t1, TMutRef t2) -> do
            let (u1, _) = numberOfRefs t1
            let (u2, _) = numberOfRefs t2
            if t1 == t2 && (u1 == TInt) && (u2 == TInt) then return TBool else throwError $ "Cannot compare " ++ show t1 ++ " with " ++ show t2
        (t1,   t2  ) -> throwError $ "Cannot compare " ++ show t1 ++ " with " ++ show t2

-- EXPRESSION TYPE CHECKER -----------------------------------------------------------

infer :: Exp -> TC Type

infer EVoid = return TUnit

-- Arithmetic
infer (EInt _) = return TInt

infer (EMul e1 e2) = arithmetic (e1, e2)
infer (EDiv e1 e2) = arithmetic (e1, e2)
infer (EAdd e1 e2) = arithmetic (e1, e2)
infer (ESub e1 e2) = arithmetic (e1, e2)

-- Booleans
infer ETrue  = return TBool
infer EFalse = return TBool

infer (ENot e) = do
    t <- infer e 
    case t of
        TBool -> return TBool
        TRef TBool -> return TBool
        TMutRef TBool -> throwError "Cannot apply unary operator ! to type &mut bool"
        _     -> throwError "Boolean operations can only be performed on booleans"
infer (EAnd e1 e2) = logic (e1, e2) 
infer (EOr  e1 e2) = logic (e1, e2) 

-- Comparisons
infer (EEq e1 e2) = do
    t1 <- infer e1
    t2 <- infer e2
    if t1 == t2 then return TBool
    else 
        let (tr1, n1) = numberOfRefs t1
            (tr2, n2) = numberOfRefs t2
        in
            if (n1 == n2 && tr1 == tr2) then return TBool
            else throwError "Cannot compare different types"
infer (ELt  e1 e2) = comparison (e1, e2)
infer (EGt  e1 e2) = comparison (e1, e2)
infer (ELeq e1 e2) = comparison (e1, e2)
infer (EGeq e1 e2) = comparison (e1, e2)

-- Control flow
-- infer (EIfElse c iff els) = do
--     cond <- infer c 
--     case cond of
--         TBool -> do
--             tI <- infer iff 
--             tE <- infer els 
--             case (tI, tE) of
--                 (tI, tE) | tI == tE -> return tI
--                 _                   -> throwError "Both branches of an if must have the same type"
--         _     -> throwError "Condition must be a boolean"

-- Let bindings
-- infer (ELet x e body) = do
--     t <- infer e 
--     withBoundVarT x (t, Imm) (infer body)
infer (EVar x) = do 
    tup <- lookupVarT x
    let vi = fst tup
    let mut = snd tup
    when (live vi == False) $ -- if vi is not live anymore
        throwError $ "Variable " ++ show x ++ "was moved"
    when ((immutableBorrows vi > 0 || mutableBorrows vi > 0) && copyFlag vi == False) $ 
        throwError $ "Cannot move out of " ++ show x ++ " because it is being borrowed by " ++ (show $ immutableBorrows vi) ++ " immutable or " ++
                     (show $ mutableBorrows vi) ++ " mutable variables" 
    when (copyFlag vi == False) $ do -- move value if variable not primitive
        -- modify (\env -> env { scopes= M.insert x (vi{ live=False },mut) (head $ scopes env) : (tail $ scopes env)} )
        scopesL . _head %= M.insert x (vi{live = False}, mut)
    return (ty vi)
    
infer (ERef exp) = do
    -- e <- infer exp
    -- (VI (TList esTy) _ live, mut) <- lookupVarT e
    case exp of
        (EVar var) -> do
            tup@(VI varTy _ live ib mb, mut) <- lookupVarT var
            when (not live) $ throwError $ "Variable " ++ show var ++ " was moved"
            when (mb /= 0) $ throwError "Cannot have immutable references while also having mutable references"
            let vi = fst tup
            -- modify (\env -> env { scopes = M.insert var (vi{ immutableBorrows = (ib+1) },mut) (head $ scopes env) : (tail $ scopes env) } )
            scopesL . _head %= M.insert var (vi{immutableBorrows = ib+1}, mut)
            -- _ <- insertInStoreT (var, fst v)
            return (TRef varTy)
        otherVal -> do
            -- let (tempVarString, g) = randomString (mkStdGen 42)
            -- a@(Addr refAddr) <- insertInStore (Ident tempVarString, Prim e)
            e <- infer exp
            return (TRef e)

infer (EMutRef exp) = do
    case exp of
        (EVar var) -> do
            tup@(VI varTy _ live ib mb, mut) <- lookupVarT var
            when (not live) $ throwError $ "Variable " ++ show var ++ " was moved"
            when (mut == Imm) $ throwError $ "Cannot borrow " ++ show var ++ " as mutable, as it is not declared as mutable"
            when (ib /= 0) $ throwError $ "Cannot borrow " ++ show var ++ " as mutable because it is also borrowed as immutable"
            when (mb /= 0) $ throwError $ "Cannot borrow " ++ show var ++ " as mutable more than once at a time"
            let vi = fst tup
            -- modify (\env -> env { scopes = M.insert var (vi{ mutableBorrows = (mb+1) }, mut) (head $ scopes env) : (tail $ scopes env) })
            scopesL . _head %= M.insert var (vi{mutableBorrows = mb+1}, mut)
            return (TMutRef varTy)
        otherVal -> do
            e <- infer exp
            return (TMutRef e)


infer (EDeref exp) = do
    tE <- infer exp
    case tE of
        (TRef rT) 
            | isCopy rT -> return rT
            | otherwise -> throwError $ "Cannot move " ++ show exp
        (TMutRef rT)  
            | isCopy rT -> return rT
            | otherwise -> throwError $ "Cannot move " ++ show exp
        _ -> throwError "Can only dereference a reference"

infer (ELight _) = return TLight

infer (EVec es) = do
    ets <- mapM infer es
    checkMultiple ets es
    case ets of
        [] -> return $ TList TUnknown -- throwError "Empty vector literal needs type annotation"
        t:ts -> 
            if (all (\ty -> (ty == t) || (fst (unwrapListTypeUntilEndNM ty 0) == TUnknown) || (fst (unwrapListTypeUntilEndNM ty 0) == TUnknown)) ts) 
                then return (TList t)
            else if (any (\ty -> isTRef ty && isMutTRef ty) ts) then return (TList t)
            else throwError $ "All elements in vector " ++ show es ++ "must be of the same type"
    where
        isTRef (TRef _) = True
        isTRef _ = False
        isMutTRef (TMutRef _) = True
        isMutTRef _ = False

infer (EIdx (EVar x) i) = do
    (VI tVec _ live ib mb, mut) <- lookupVarT x
    (TList elTy) <- case tVec of
                      s@(TList eT) -> do
                                        case isCopy eT of
                                            True -> return s
                                            False -> if mb /= 0 then throwError ("Cannot borrow " ++ show x ++ " as immutable as it is also borrowed as mutable")
                                                     else return s
                      (TRef t) -> maxUnwrapType tVec
                      (TMutRef t) -> maxUnwrapType tVec
                      _ -> throwError $ "Cannot index non-arrays " ++ show x
    tIdx <- infer i
    when (tIdx /= TInt) $ throwError "Array index must be an integer"
    return elTy

infer (EIdx v@(EVec es) i) = do
    tIdx <- infer i
    when (tIdx /= TInt) $ throwError "Array index must be an integer"
    (TList eTy) <- infer v
    -- if isCopy eTy then return eTy
    -- else throwError "Cannot move non-copyable type from list"
    return eTy

infer (EIdx prevE@(EIdx v prevI) i) = do
    tIdx <- infer i
    when (tIdx /= TInt) $ throwError "Array index must be an integer"
    tp <- infer prevE
    case tp of
        TList elTy -> return elTy
            -- | isCopy elTy -> return elTy
            -- | otherwise -> throwError "Cannot move non-copyable type from list"
        _ -> throwError "Indexing works only on lists"

infer (EIdx rmv@(ERemove vc ri) i) = do
    tIdx <- infer i
    when (tIdx /= TInt) $ throwError "Array index must be an integer"
    tp <- infer rmv
    case tp of
        TList elTy -> return elTy
        _ -> throwError "Indexing works only on lists"

infer (EIdx r@(ERef ref) i) = do
    tIdx <- infer i
    when (tIdx /= TInt) $ throwError "Array index must be an integer"
    tp <- infer r
    liftIO $ print tp
    unwrapped <- maxUnwrapType tp
    liftIO $ print unwrapped
    case unwrapped of 
        TList elTy -> return elTy
        _ -> throwError "Indexing works only on lists"

infer (EIdx r@(EMutRef ref) i) = do
    tIdx <- infer i
    when (tIdx /= TInt) $ throwError "Array index must be an integer"
    tp <- infer r
    liftIO $ print tp
    unwrapped <- maxUnwrapType tp
    liftIO $ print unwrapped
    case unwrapped of 
        TList elTy -> return elTy
        _ -> throwError "Indexing works only on lists"

infer (EIdx d@(EDeref e) i) = do
    tIdx <- infer i
    when (tIdx /= TInt) $ throwError "Array index must be an integer"
    tp <- infer d
    unwrapped <- maxUnwrapType tp
    case unwrapped of
        TList elTy -> return elTy
        _ -> throwError ""
    

infer (ERemove (EVar x) i) = do
    tVec <- peekVarType x
    (TList elTy) <- case tVec of
                      s@(TList _) -> return s
                      (TRef t) -> throwError $ "Set: Cannot borrow " ++ show x ++ " as mutable, as it is behind a & reference" -- SYNTACTICAL SUGAR
                      -- (TMutRef t) -> if mut == Imm then throwError $ "Set: Cannot borrow " ++ show vec ++ " as mutable, as it is behind a & reference"
                      --             else maxUnwrapType t
                      _ -> throwError "The remove function works only on lists"
    tIdx <- infer i
    when (tIdx /= TInt) $ throwError "Remove: Array index must be an integer"
    return elTy

infer (ERemove rmv@(ERemove vc ri) i) = do
    tIdx <- infer i
    when (tIdx /= TInt) $ throwError "Array index must be an integer"
    tp <- infer rmv
    case tp of
        TList elTy -> return elTy
        _ -> throwError  "Indexing works only on lists"

-- infer (ERemove v@(EVec es) i) = do
--     tIdx <- infer i
--     when (tIdx /= TInt) $ throwError "Array index must be an integer"
--     (TList eTy) <- infer v
--     return eTy

infer (ERemove prevE@(EIdx v prevI) i) = do
    tIdx <- infer i
    when (tIdx /= TInt) $ throwError "Array index must be an integer"
    tp <- infer prevE
    case tp of
        TList elTy -> return elTy
        _ -> throwError "Can only remove an element from a list"


-- infer (EPush vec el) = do
--     (TList esTy) <- infer vec
--     eTy <- infer el
    
--     if eTy == esTy then return TUnit 
--     else throwError $ "Push: List's " ++ show vec ++ " elements are of type " ++ show esTy ++ ", but " ++ show el ++ " is of type " ++ show eTy

-- Functions
infer (EApp f args) = do
    (TFun paramsT_Muts retTy) <- lookupFunT f
    -- done by both the typechecker and the interpreter
    when (length paramsT_Muts /= length args) $
       throwError $ "function " ++ show f ++ " expects " ++ show (length paramsT_Muts) ++ " arguments"
    
    forM_ (zip args paramsT_Muts) $ \(arg, expected) -> do
        e <- get
        actualT <- infer arg
        let expectedT = fst expected
        case (expectedT, arg) of
            (TRef _, EVar var) -> do
                tup@(VI ttt _ live ib mb, mut) <- lookupVarT var
                when (mb /= 0) $ throwError $ "Cannot borrow " ++ show var ++ " as immutable as it is also borrowed as mutable"
                let vi = fst tup
                changeVarTIB var (ib+1)
            (TMutRef _, EVar var) -> do
                tup@(VI ttt _ live ib mb, mut) <- lookupVarT var
                when (mb /= 0) $ throwError $ "Cannot borrow " ++ show var ++ " as mutable more than once"
                when (ib /= 0) $ throwError $ "Cannot borrow " ++ show var ++ " as mutable as it is also borrowed as immutable"
                let vi = fst tup
                changeVarTMB var (mb+1)
            (_, _) -> return ()
        -- let realT = case actualT of
        --         TList TUnknown -> expectedT
        --         other -> other

  -- for nested arrays that might be empty
        realUnwrappedType <- 
            case (actualT, expectedT) of
                (TList rt, TList tty) -> do
                    tup <- unwrapListTypeUntilEnd (TList rt) 0
                    let realUnwrapped = fst tup
                    let numberOfUnwrapsReal = snd tup

                    tup2 <- unwrapListTypeUntilEnd (TList tty) 0
                    let expectedUnwrapped = fst tup2
                    let numberOfUnwrapsExpected = snd tup2
                    if (numberOfUnwrapsReal == numberOfUnwrapsExpected) then
                        case realUnwrapped of 
                            TUnknown -> wrapTypeBack expectedUnwrapped numberOfUnwrapsReal
                            other -> wrapTypeBack other numberOfUnwrapsReal
                    else 
                        throwError $ "Type mismatch in call to " ++ show f ++
                                    ": expected " ++ show expected ++
                                    " but got " ++ show actualT
                                    
                (a, e) -> return a
        checkMoveNotAllowed realUnwrappedType arg
        when (not (fitsInto realUnwrappedType expectedT)) $ throwError $ 
            "type mismatch in call to " ++ show f ++
            ": expected " ++ show expected ++
            " but got " ++ show realUnwrappedType

    return retTy

unwrapListTypeUntilEnd :: Type -> Int -> TC (Type, Int)
unwrapListTypeUntilEnd (TList listT) n = unwrapListTypeUntilEnd listT (n+1)
unwrapListTypeUntilEnd something n = return (something, n)

unwrapListTypeUntilEndNM :: Type -> Int -> (Type, Int)
unwrapListTypeUntilEndNM (TList listT) n = unwrapListTypeUntilEndNM listT (n+1)
unwrapListTypeUntilEndNM something n = (something, n)

wrapTypeBack :: Type -> Int -> TC Type
wrapTypeBack t 0 = return t
wrapTypeBack t n = wrapTypeBack (TList t) (n-1)

peekVarType :: Ident -> TC Type
peekVarType x = do
  (vi, _) <- lookupVarT x
  when (live vi == False) $
      throwError $ "Variable " ++ show x ++ " was moved"
  return (ty vi)