module TypeCheck.Stmt where

import Evaluator

import Env hiding (modify)

import Value ( TClosure( TFun )
             , Mutability( Imm, Mut )
             , VarInfo(..)
             , isCopy
             , fitsInto )

import Lang.Abs ( Stmt(..)
                , Type(..)
                , Ident
                , Arg(..)
                , Exp(..), IndexList(..) )

import qualified TypeCheck.Expr as E
import qualified Data.Map.Strict as M
import Data.List (foldl')
import Control.Monad
import Control.Monad.Trans.State.Strict
import Control.Lens

-- STATEMENT TYPE CHECKER ------------------------------------------------------------

voidNotAllowed :: Type -> Exp -> Stmt -> TC ()
voidNotAllowed t e st = when (t == TUnit) $ throwError ("Type cannot be unit in expression: " ++ show e ++ "\nIn statement: " ++ show st)

-- moveNotAllowed :: Type -> Exp -> Stmt -> TC ()
-- moveNotAllowed (TList eTy) e st 
--   | isCopy eTy = return ()
--   | otherwise  = throwError ("Cannot move element from array in expression " ++ show e ++ "\nIn statement: " ++ show st)
-- moveNotAllowed _ _ _ = return ()

checkMoveNotAllowed :: Type -> Exp -> TC ()
checkMoveNotAllowed t e@(EIdx vec i)
  | isCopy t = return ()
  | otherwise= throwError ("Cannot move element from array in expression " ++ show e)
checkMoveNotAllowed _ _ = return ()

infer :: Stmt -> TC ()

createVi :: Type -> Bool -> Bool -> Int -> Int -> VarInfo
createVi = VI

infer s@(SLet x e) = do
    t <- E.infer e
    realT <- case t of
              TList TUnknown -> throwError "Empty vector literal needs type annotation"
              other -> return other
    case realT of
      TList t -> do
        tup <- unwrapListTypeUntilEnd (TList t) 0
        when (fst tup == TUnknown) $ throwError "Empty vector literal needs type annotation"
      _ -> return ()
    checkMoveNotAllowed realT e
    let c = isCopy realT
    voidNotAllowed realT e s
    insertVarT x (createVi realT c True 0 0, Imm)

-- change names of types
infer s@(SLetAnn x ty e) = do
  t <- E.infer e

  -- for arrays that might be empty
  -- let realT = case t of
  --               TList TUnknown -> ty
  --               other -> other

  -- for nested arrays that might be empty
  realUnwrappedType <- case (t, ty) of
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
                      throwError $ "Type mismatch for " ++ show x ++
                                  ": annotation says " ++ show ty ++
                                  " but expression has type " ++ show t

                  (a, e) -> return a

  checkMoveNotAllowed realUnwrappedType e
  let c = isCopy realUnwrappedType
  voidNotAllowed realUnwrappedType e s
  if (fitsInto realUnwrappedType ty)
     then insertVarT x (createVi realUnwrappedType c True 0 0, Imm) -- >> do {e <- get; (liftIO $ print (scopes e))}
     else throwError $
       "Type mismatch for " ++ show x ++
       ": annotation says " ++ show ty ++
       " but expression has type " ++ show realUnwrappedType

infer s@(SLetM x e) = do
    t <- E.infer e
    realT <- case t of
              TList TUnknown -> throwError "Empty vector literal needs type annotation"
              other -> return other
    case realT of
      TList t -> do
        tup <- unwrapListTypeUntilEnd (TList t) 0
        when (fst tup == TUnknown) $ throwError "Empty vector literal needs type annotation"
      _ -> return ()

    checkMoveNotAllowed t e
    let c = isCopy t
    voidNotAllowed t e s
    insertVarT x (createVi t c True 0 0, Mut)

-- change names of types
infer s@(SLetMAnn x ty e) = do
  t <- E.infer e
  -- let realT = case t of
  --               TList TUnknown -> ty
  --               other -> other

  -- for nested arrays that might be empty
  realUnwrappedType <- case (t, ty) of
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
                      throwError $ "Type mismatch for " ++ show x ++
                                  ": annotation says " ++ show ty ++
                                  " but expression has type " ++ show t

                  (a, e) -> return a

  checkMoveNotAllowed realUnwrappedType e
  let c = isCopy realUnwrappedType
  voidNotAllowed realUnwrappedType e s
  if (fitsInto realUnwrappedType ty)
     then insertVarT x (createVi realUnwrappedType c True 0 0, Mut)
     else throwError $
       "Type mismatch for " ++ show x ++
       ": annotation says " ++ show ty ++
       " but expression has type " ++ show realUnwrappedType

-- infer (SFunNp f retTy stmts lastE) = do

infer (SFun f params retTy stmts lastE) = do
  let bindInfo = [ (name, (VI t (isCopy t) True 0 0, if isMut then Mut else Imm))
               | p <- params
               , let (name, t, isMut) = case p of
                       ArgImm nm ty -> (nm , ty , False)
                       ArgMut nm ty -> (nm , ty , True) ]

  let argst = map (\(_,(t,_)) -> t) bindInfo
  let argMuts = map (\(_,(_,m)) -> m) bindInfo
  let paramScope = M.fromList bindInfo
  outerEnv <- get
  put outerEnv { scopes = [paramScope] }

  tBody <- do
              inferAndPass stmts
              E.infer lastE

  checkMoveNotAllowed tBody lastE

  put outerEnv

  when (tBody /= retTy) $
      throwError $ "return type mismatch in " ++ show f ++
        ": annotation says " ++ show retTy ++
        ", but body returns " ++ show tBody
  let types = map ty argst
  insertFunT f (TFun (zip types argMuts) retTy)


-- Currently cannot use functions as first class values
infer s@(SAss x e) = do
  (vi, mut) <- lookupVarT x
  -- when (live vi == False) $
  --     throwError $ "Variable " ++ show x ++ " was moved and cannot be assigned"
  case mut of
    Imm -> throwError ("cannot assign to immutable " ++ show x)
    Mut -> do
      ety <- E.infer e
      realUnwrappedType <- case (ety, ty vi) of
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
                      throwError $ "Type mismatch for " ++ show x ++
                                  ": annotation says " ++ show (ty vi) ++
                                  " but expression has type " ++ show ety

                  (a, e) -> return a
      voidNotAllowed realUnwrappedType e s
      checkMoveNotAllowed realUnwrappedType e
      if (fitsInto realUnwrappedType (ty vi)) then do
        let c = isCopy realUnwrappedType
        let vi' = vi { live = True }
        -- modify (\env -> env { scopes= M.insert x (vi', Mut) (head $ scopes env) : (tail $ scopes env)} )
        scopesL . _head %= M.insert x (vi', Mut)
        return ()
      else throwError $ "Type mismatch in assignment to " ++ show x
                      ++ ": variable has type " ++ show (ty vi)
                      ++ ", but expression has type " ++ show realUnwrappedType

infer w@(SWhile cond stmts) = do
  e <- get
  -- liftIO $ print (scopes e)
  tCond <- E.infer cond
  case tCond of
    TBool -> withScopeT (inferAndPass stmts)
    _     -> throwError $ "Condition does not have bool type in while statement " ++ show w

infer i@(SIf cond stmts) = do
  cont <- E.infer cond
  case cont of
    TBool -> withScopeT (inferAndPass stmts)
    _     -> throwError $ "Condition does not have bool type in if statement " ++ show i

infer i@(SIfElse cond thn els) = do
    cont <- E.infer cond
    case cont of
      TBool -> withScopeT (inferAndPass thn) >> withScopeT (inferAndPass els)
      _     -> throwError $ "Condition does not have bool type in if statement " ++ show i

infer s@(SPush (EVar vec) el) = do
    -- (TList esTy) <- infer vec
    (VI tp _ live _ _, mut) <- lookupVarT vec
    when (not live) $ throwError $ "List " ++ show vec ++ " was moved"
    (TList esTy) <- case tp of
                      s@(TList _) -> return s
                      (TRef t) -> throwError $ "Set: Cannot borrow " ++ show vec ++ " as mutable, as it is behind a & reference"
                      -- (TMutRef t) -> if mut == Imm then throwError $ "Set: Cannot borrow " ++ show vec ++ " as mutable, as it is behind a & reference"
                      --             else maxUnwrapType t
                      _ -> throwError $ "Cannot use the push function for arrays on non-arrays " ++ show vec
    when (mut == Imm) $ throwError $ "List " ++ show vec ++ " is immutable"
    eTy <- E.infer el
    checkMoveNotAllowed eTy el
    if fitsInto eTy esTy then return ()
    else throwError $ "Push: List's " ++ show vec ++ " elements are of type " ++ show esTy ++ ", but element " ++ show el ++ " has type " ++ show eTy

-- infer s@(SPush v@(EVec es) el) = do
--     eTy <- E.infer el
--     checkMoveNotAllowed eTy el
--     (TList esTy) <- E.infer v
--     when (eTy /= esTy) $ throwError $ "Push: List's " ++ show v ++ " elements are of type " ++ show esTy ++ ", but element " ++ show el ++ " has type " ++ show eTy

infer s@(SPush prevE@(EIdx v prevI) el) = do
    eTy <- E.infer el
    checkMoveNotAllowed eTy el
    (TList esTy) <- E.infer prevE
    when (not $ fitsInto eTy esTy) $ throwError $ "Push: List's " ++ show v ++ " elements are of type " ++ show esTy ++ ", but element " ++ show el ++ " has type " ++ show eTy


infer s@(SInsert (EVar vec) i el) = do
  (VI tp _ live _ _, mut) <- lookupVarT vec
  when (not live) $ throwError $ "List " ++ show vec ++ " was moved"
  (TList esTy) <- case tp of
                      s@(TList _) -> return s
                      (TRef t) -> throwError $ "Set: Cannot borrow " ++ show vec ++ " as mutable, as it is behind a & reference"
                      -- (TMutRef t) -> if mut == Imm then throwError $ "Set: Cannot borrow " ++ show vec ++ " as mutable, as it is behind a & reference"
                      --             else maxUnwrapType t
                      _ -> throwError $ "Cannot use the insert function for arrays on non-arrays " ++ show vec
  -- how about changing this to lookupVarT (done)? or to peekVarType back
  idxTy <- E.infer i
  when (idxTy /= TInt) $ throwError "Index to insert element in list must be an integer"

  when (mut == Imm) $ throwError $ "List " ++ show vec ++ " is immutable"
  eTy <- E.infer el 
  checkMoveNotAllowed eTy el
  if fitsInto eTy esTy then return ()
  else throwError $ "Insert: List's " ++ show vec ++ " elements are of type " ++ show esTy ++ ", but element " ++ show el ++ " has type " ++ show eTy

infer s@(SInsert prevE@(EIdx v prevI) i el) = do
    idxTy <- E.infer i
    when (idxTy /= TInt) $ throwError "Index to insert element in list must be an integer"
    eTy <- E.infer el
    checkMoveNotAllowed eTy el
    (TList esTy) <- E.infer prevE
    when (not $ fitsInto eTy esTy) $ throwError $ "Push: List's " ++ show v ++ " elements are of type " ++ show esTy ++ ", but element " ++ show el ++ " has type " ++ show eTy

-- error message if not EVar vec
infer s@(SSetIdx (EVar vec) idxs el) = do
    (VI tp _ live _ _, mut) <- lookupVarT vec
    when (not live) $ throwError $ "List " ++ show vec ++ " was moved"
    (TList esTy) <- case tp of
                      s@(TList _) -> return s
                      (TRef t) -> throwError $ "Set: Cannot borrow " ++ show vec ++ " as mutable, as it is behind a & reference"
                      -- (TMutRef t) -> if mut == Imm then throwError $ "Set: Cannot borrow " ++ show vec ++ " as mutable, as it is behind a & reference"
                      --             else maxUnwrapType t
                      (TMutRef t) -> maxUnwrapType t
                      _ -> throwError $ "Cannot use the set function for arrays on non-arrays " ++ show vec
    let exps = [e | IndexList e <- idxs]
    idxsTy <- mapM E.infer exps
    when (any (\idxTy -> idxTy /= TInt) idxsTy) $ throwError "Index to insert element at must be an integer"
    when (mut == Imm) $ throwError $ "List " ++ show vec ++ " is immutable"
    eTy <- E.infer el
    realUnwrappedType <- case (eTy, esTy) of
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
                      throwError $
                      "Set: List's " ++ show vec ++ " elements are of type " ++ show esTy ++ ", but element " ++ show el ++ " has type " ++ show eTy

                  (a, e) -> return a
    checkMoveNotAllowed realUnwrappedType el
    unwrappedType <- unwrapListType (TList esTy) (length idxs)
    when (not $ fitsInto realUnwrappedType unwrappedType) $
      throwError $ "Set: List's " ++ show vec ++ " elements are of type " ++ show esTy ++ ", but element " ++ show el ++ " has type " ++ show eTy

infer s@(SSetIdx d@(EDeref ref) idxs el) = do
  refTy <- E.infer ref
  (innerListTy, esTy) <- case refTy of
      TMutRef t@(TList es) -> return (t, es)
      TRef _ -> throwError $ "Set: cannot modify through immutable reference " ++ show ref
      _ -> throwError $ "Set: " ++ show ref ++ " is not a reference to a list"
  
  let idxEs = [ e | IndexList e <- idxs ]
  mapM_ (\idx -> do ixTy <- E.infer idx
                    when (ixTy /= TInt) $ throwError "Set: every index must have type int")
        idxEs

  eTy <- E.infer el
  realUnwrappedType <- case (eTy, esTy) of
        (TList rt, TList ety) -> do
            (real, nReal) <- unwrapListTypeUntilEnd (TList rt) 0
            (expd, nExpd) <- unwrapListTypeUntilEnd (TList ety) 0
            if nReal == nExpd
                 then wrapTypeBack (if real == TUnknown then expd else real) nReal
                 else throwError $
                       "Set: List's " ++ show ref ++ " elements are of type " ++ show esTy ++ ", but element " ++ show el ++ " has type " ++ show eTy
        _ -> return eTy
  
  checkMoveNotAllowed realUnwrappedType el
  
  wantedTy <- unwrapListType innerListTy (length idxs)
  when (not $ fitsInto realUnwrappedType wantedTy) $ 
    throwError $ "Set: element type mismatch. List has " ++ show wantedTy ++ ", expression has " ++ show realUnwrappedType
    

-- infer s@(SSetIdx e@(EDeref vec) )

infer (SExp e) = do
    E.infer e
    return ()

infer (SAssDeref e v) = do
  eT <- E.infer e
  newValT <- E.infer v
  case eT of
    (TRef _) -> throwError "Cannot mutate value under immutable reference"
    (TMutRef rT) -> when (rT /= newValT) $ throwError $ "Value under reference has type " ++ show rT ++ ", but expression " ++ show v ++ "has type " ++ show newValT
    _ -> throwError $ show e ++ " is not a mutable reference (&mut)"

unwrapListTypeUntilEnd :: Type -> Int -> TC (Type, Int)
unwrapListTypeUntilEnd (TList listT) n = unwrapListTypeUntilEnd listT (n+1)
unwrapListTypeUntilEnd something n = return (something, n)

wrapTypeBack :: Type -> Int -> TC Type
wrapTypeBack t 0 = return t
wrapTypeBack t n = wrapTypeBack (TList t) (n-1)

unwrapListType :: Type -> Int -> TC Type
unwrapListType something 0 = return something
unwrapListType (TList listT) idxsLeft = unwrapListType listT (idxsLeft - 1)

-- infer s@(SSetIdx (EVar vec) i el) = do
--   (VI (TList esTy) _ live, mut) <- lookupVarT vec
--   when (not live) $ throwError $ "List " ++ show vec ++ " was moved"

--   idxTy <- E.infer i
--   when (idxTy /= TInt) $ throwError "Index to insert element in list must be an integer"

--   when (mut == Imm) $ throwError $ "List " ++ show vec ++ " is immutable"
--   eTy <- E.infer el
--   checkMoveNotAllowed eTy el
--   if eTy == esTy then return ()
--   else throwError $ "Insert: List's " ++ show vec ++ " elements are of type " ++ show esTy ++ ", but element " ++ show el ++ " has type " ++ show eTy

-- infer s@(SSetIdx prevE@(EIdx v prevI) i el) = do
--     idxTy <- E.infer i
--     when (idxTy /= TInt) $ throwError "Index to insert element in list must be an integer"
--     eTy <- E.infer el
--     checkMoveNotAllowed eTy el
--     (TList esTy) <- E.infer prevE
--     when (eTy /= esTy) $ throwError $ "Push: List's " ++ show v ++ " elements are of type " ++ show esTy ++ ", but element " ++ show el ++ " has type " ++ show eTy

-- peekVarType :: Ident -> TC Type
-- peekVarType x = do
--   (vi, _) <- lookupVarT x
--   when (live vi == False) $
--       throwError $ "Variable " ++ show x ++ " was moved"
--   return (ty vi)

inferAndPass :: [Stmt] -> TC ()
inferAndPass [] = return ()
inferAndPass stmts = do
    let stmt = head stmts
    infer stmt
    inferAndPass (tail stmts)
