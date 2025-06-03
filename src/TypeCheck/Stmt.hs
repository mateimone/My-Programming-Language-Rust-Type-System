module TypeCheck.Stmt where

import Evaluator

import Env hiding (modify)

import Value ( TClosure( TFun )
             , Mutability( Imm, Mut )
             , VarInfo(..)
             , isCopy )

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

createVi :: Type -> Bool -> Bool -> VarInfo
createVi = VI

infer s@(SLet x e) = do
    t <- E.infer e
    checkMoveNotAllowed t e
    let c = isCopy t
    voidNotAllowed t e s
    insertVarT x (createVi t c True, Imm)

infer s@(SLetAnn x ty e) = do
  t <- E.infer e
  checkMoveNotAllowed t e
  let c = isCopy t
  voidNotAllowed t e s
  if t == ty
     then insertVarT x (createVi t c True, Imm) -- >> do {e <- get; (liftIO $ print (scopes e))}
     else throwError $
       "Type mismatch for " ++ show x ++
       ": annotation says " ++ show ty ++
       " but expression has type " ++ show t

infer s@(SLetM x e) = do
    t <- E.infer e
    checkMoveNotAllowed t e
    let c = isCopy t
    voidNotAllowed t e s
    insertVarT x (createVi t c True, Mut)

infer s@(SLetMAnn x ty e) = do
  t <- E.infer e
  checkMoveNotAllowed t e
  let c = isCopy t
  voidNotAllowed t e s
  if t == ty
     then insertVarT x (createVi t c True, Mut)
     else throwError $
       "Type mismatch for " ++ show x ++
       ": annotation says " ++ show ty ++
       " but expression has type " ++ show t

-- infer (SFunNp f retTy stmts lastE) = do

infer (SFun f params retTy stmts lastE) = do
  let bindInfo = [ (name, (VI t (isCopy t) True, if isMut then Mut else Imm))
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
  insertFunT f (TFun (zip types argMuts) retTy, Imm)


-- Currently cannot use functions as first class values
infer s@(SAss x e) = do
  (vi, mut) <- lookupVarT x
  -- when (live vi == False) $
  --     throwError $ "Variable " ++ show x ++ " was moved and cannot be assigned"
  case mut of
    Imm -> throwError ("cannot assign to immutable " ++ show x)
    Mut -> do
      ety <- E.infer e
      voidNotAllowed ety e s
      checkMoveNotAllowed ety e
      if (ty vi) == ety then do
        let c = isCopy ety
        let vi' = vi { live = True }
        modify (\env -> env { scopes= M.insert x (vi', Mut) (head $ scopes env) : (tail $ scopes env)} )
        return ()
      else throwError $ "Type mismatch in assignment to " ++ show x
                      ++ ": variable has type " ++ show (ty vi)
                      ++ ", but expression has type " ++ show ety

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
    (VI (TList esTy) _ live, mut) <- lookupVarT vec
    when (not live) $ throwError $ "List " ++ show vec ++ " was moved"
    when (mut == Imm) $ throwError $ "List " ++ show vec ++ " is immutable"
    eTy <- E.infer el
    checkMoveNotAllowed eTy el
    if eTy == esTy then return ()
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
    when (eTy /= esTy) $ throwError $ "Push: List's " ++ show v ++ " elements are of type " ++ show esTy ++ ", but element " ++ show el ++ " has type " ++ show eTy


infer s@(SInsert (EVar vec) i el) = do
  (VI (TList esTy) _ live, mut) <- lookupVarT vec
  when (not live) $ throwError $ "List " ++ show vec ++ " was moved"

  -- how about changing this to lookupVarT (done)? or to peekVarType back
  idxTy <- E.infer i
  when (idxTy /= TInt) $ throwError "Index to insert element in list must be an integer"

  when (mut == Imm) $ throwError $ "List " ++ show vec ++ " is immutable"
  eTy <- E.infer el
  checkMoveNotAllowed eTy el
  if eTy == esTy then return ()
  else throwError $ "Insert: List's " ++ show vec ++ " elements are of type " ++ show esTy ++ ", but element " ++ show el ++ " has type " ++ show eTy

infer s@(SInsert prevE@(EIdx v prevI) i el) = do
    idxTy <- E.infer i
    when (idxTy /= TInt) $ throwError "Index to insert element in list must be an integer"
    eTy <- E.infer el
    checkMoveNotAllowed eTy el
    (TList esTy) <- E.infer prevE
    when (eTy /= esTy) $ throwError $ "Push: List's " ++ show v ++ " elements are of type " ++ show esTy ++ ", but element " ++ show el ++ " has type " ++ show eTy

infer s@(SSetIdx (EVar vec) idxs el) = do
    (VI (TList esTy) _ live, mut) <- lookupVarT vec
    let exps = [e | IndexList e <- idxs]
    idxsTy <- mapM E.infer exps
    when (any (\idxTy -> idxTy /= TInt) idxsTy) $ throwError "Index to insert element in list must be an integer"
    when (mut == Imm) $ throwError $ "List " ++ show vec ++ " is immutable"
    eTy <- E.infer el
    checkMoveNotAllowed eTy el
    -- liftIO $ print (TList esTy)
    -- liftIO $ print (length idxs)
    unwrappedType <- unwrapListType (TList esTy) (length idxs)
    -- liftIO $ print unwrappedType
    when (eTy /= unwrappedType) $ 
      throwError $ "Set: List's " ++ show vec ++ " elements are of type " ++ show esTy ++ ", but element " ++ show el ++ " has type " ++ show eTy
    
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
