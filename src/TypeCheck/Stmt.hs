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

-- Abort if an expression's type is Unit (our language disallows void values
-- in "value position"). The extra Stmt parameter is only for a nicer
-- error-message that points at the surrounding statement.
voidNotAllowed :: Type -> Exp -> Stmt -> TC ()
voidNotAllowed t e st = when (t == TUnit) $ throwError ("Type cannot be unit in expression: " ++ show e ++ "\nIn statement: " ++ show st)

-- Prevent moving a non-copy element out of a list
checkMoveNotAllowed :: Type -> Exp -> TC ()
checkMoveNotAllowed t e@(EIdx vec i)
  | isCopy t = return ()
  | otherwise= throwError ("Cannot move element from array in expression " ++ show e)
checkMoveNotAllowed _ _ = return ()

createVi :: Type -> Bool -> Int -> Int -> VarInfo
createVi = VI

infer :: Stmt -> TC ()

-- SLet - create an immutable variable, give a value to it
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
    voidNotAllowed realT e s
    insertVarT x (createVi realT True 0 0, Imm)

-- SLetAnn - create an immutable variable with an explicit type annotation, give a value to it
infer s@(SLetAnn x ty e) = do
  t <- E.infer e

  -- for nested arrays that might be empty
  realUnwrappedType <- 
    reconcileListLike t ty ("Type mismatch for " ++ show x ++ ": annotation says " ++ show ty ++ " but expression has type " ++ show t)

  checkMoveNotAllowed realUnwrappedType e
  voidNotAllowed realUnwrappedType e s
  if (fitsInto realUnwrappedType ty)
    then insertVarT x (createVi realUnwrappedType True 0 0, Imm)
    else throwError $
      "Type mismatch for " ++ show x ++
      ": annotation says " ++ show ty ++
      " but expression has type " ++ show realUnwrappedType

-- SLetM - create a mutable variable, give a value to it
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
    voidNotAllowed t e s
    insertVarT x (createVi t True 0 0, Mut)

-- SLetMAnn - create a mutable variable with an explicit type annotation, give a value to it
infer s@(SLetMAnn x ty e) = do
  t <- E.infer e

  -- for nested arrays that might be empty
  realUnwrappedType <- 
    reconcileListLike t ty ("Type mismatch for " ++ show x ++ ": annotation says " ++ show ty ++ " but expression has type " ++ show t)

  checkMoveNotAllowed realUnwrappedType e
  voidNotAllowed realUnwrappedType e s
  if (fitsInto realUnwrappedType ty)
    then insertVarT x (createVi realUnwrappedType True 0 0, Mut)
    else throwError $
      "Type mismatch for " ++ show x ++
      ": annotation says " ++ show ty ++
      " but expression has type " ++ show realUnwrappedType

-- SFun - function definition. We create a fresh scope for the parameters,
-- type-check the body, then restore the outer environment.
infer (SFun f params retTy stmts lastE) = do
  let bindInfo = [ (name, (VI t True 0 0, if isMut then Mut else Imm))
              | p <- params
              , let (name, t, isMut) = case p of
                      ArgImm nm ty -> (nm , ty , False)
                      ArgMut nm ty -> (nm , ty , True) ]

  let argst = map (\(_,(t,_)) -> t) bindInfo
  let argMuts = map (\(_,(_,m)) -> m) bindInfo
  let paramScope = M.fromList bindInfo
  outerEnv <- get
  -- put outerEnv { scopes = [paramScope] }
  scopesL .= [paramScope]

  tBody <- do
              inferAndPass stmts
              E.infer lastE

  put outerEnv

  checkMoveNotAllowed tBody lastE

  when (tBody /= retTy) $
      throwError $ "return type mismatch in " ++ show f ++
        ": annotation says " ++ show retTy ++
        ", but body returns " ++ show tBody
  let types = map ty argst
  insertFunT f (TFun (zip types argMuts) retTy)

-- SAss - assign a value to a variable 
infer s@(SAss x e) = do
  (vi, mut) <- lookupVarT x
  case mut of
    Imm -> throwError ("cannot assign to immutable " ++ show x)
    Mut -> do
      eTy <- E.infer e
      realUnwrappedType <- 
        reconcileListLike eTy (ty vi) ("Type mismatch for " ++ show x ++ ": annotation says " ++ show (ty vi) ++ " but expression has type " ++ show eTy)

      voidNotAllowed realUnwrappedType e s
      checkMoveNotAllowed realUnwrappedType e
      if (fitsInto realUnwrappedType (ty vi)) then do
        let c = isCopy realUnwrappedType
        let vi' = vi { live = True }
        scopesL . _head %= M.insert x (vi', Mut)
        return ()
      else throwError $ "Type mismatch in assignment to " ++ show x
                      ++ ": variable has type " ++ show (ty vi)
                      ++ ", but expression has type " ++ show realUnwrappedType

-- SArtBlock - artificial "{}" block: any bindings are dropped at the end of this block
infer (SArtBlock stmts) = do
    withScopeT (inferAndPass stmts)

-- SWhile: while loop, any new bindings are dropped at the end
infer w@(SWhile cond stmts) = do
  e <- get
  tCond <- E.infer cond
  case tCond of
    TBool -> withScopeT (inferAndPass stmts)
    _     -> throwError $ "Condition does not have bool type in while statement " ++ show w

-- SIf: if statement, any new bindings are dropped at the end
infer i@(SIf cond stmts) = do
  cont <- E.infer cond
  case cont of
    TBool -> withScopeT (inferAndPass stmts)
    _     -> throwError $ "Condition does not have bool type in if statement " ++ show i

-- SIfElse: if-else statement, any new bindings are dropped at the end; 
-- the environments of the two branches are separate
infer i@(SIfElse cond thn els) = do
    cont <- E.infer cond
    case cont of
      TBool -> withScopeT (inferAndPass thn) >> withScopeT (inferAndPass els)
      _     -> throwError $ "Condition does not have bool type in if statement " ++ show i


-- SPush cases --

-- Push called by variable
infer s@(SPush (EVar vec) el) = do
    (VI tp live _ _, mut) <- lookupVarT vec
    when (not live) $ throwError $ "List " ++ show vec ++ " was moved"
    (TList esTy) <- case tp of
                      s@(TList _) -> return s
                      (TRef t) -> throwError $ "Set: Cannot borrow " ++ show vec ++ " as mutable, as it is behind a & reference" -- Syntactical sugar
                      (TMutRef t) -> maxUnwrapType t -- Syntactical sugar
                      _ -> throwError $ "Cannot use the push function for arrays on non-arrays " ++ show vec
    when (mut == Imm) $ throwError $ "List " ++ show vec ++ " is immutable"
    eTy <- E.infer el
    checkMoveNotAllowed eTy el
    if fitsInto eTy esTy then return ()
    else throwError $ "Push: List's " ++ show vec ++ " elements are of type " ++ show esTy ++ ", but element " ++ show el ++ " has type " ++ show eTy

-- Push called by an indexed element
infer s@(SPush prevE@(EIdx v prevI) el) = do
    eTy <- E.infer el
    checkMoveNotAllowed eTy el
    (TList esTy) <- E.infer prevE
    when (not $ fitsInto eTy esTy) $ throwError $ "Push: List's " ++ show v ++ " elements are of type " ++ show esTy ++ ", but element " ++ show el ++ " has type " ++ show eTy

-- Push called by a dereferenced element
infer s@(SPush (EDeref ref) el) = do
  refTy <- case ref of
            EVar v -> E.peekVarType v
            _ -> E.infer ref

  esTy <- case refTy of
    TMutRef (TList ty) -> return ty
    TRef _ -> throwError $ "Push: cannot modify through immutable reference " ++ show ref
    _ -> throwError $ "Push: " ++ show ref ++ " is not a reference to a list"

  elTy <- E.infer el
  checkMoveNotAllowed elTy el
  unless (fitsInto elTy esTy) $
    throwError $ "Push: list element type is " ++ show esTy ++
                ", but pushed value has type " ++ show elTy


-- SInsert cases --

-- Insert called by variable
infer s@(SInsert (EVar vec) i el) = do
  (VI tp live _ _, mut) <- lookupVarT vec
  when (not live) $ throwError $ "List " ++ show vec ++ " was moved"
  (TList esTy) <- case tp of
                      s@(TList _) -> return s
                      (TRef t) -> throwError $ "Set: Cannot borrow " ++ show vec ++ " as mutable, as it is behind a & reference"
                      (TMutRef t) -> maxUnwrapType t
                      _ -> throwError $ "Cannot use the insert function for arrays on non-arrays " ++ show vec
  idxTy <- E.infer i
  when (idxTy /= TInt) $ throwError "Index to insert element in list must be an integer"

  when (mut == Imm) $ throwError $ "List " ++ show vec ++ " is immutable"
  eTy <- E.infer el 
  checkMoveNotAllowed eTy el
  if fitsInto eTy esTy then return ()
  else throwError $ "Insert: List's " ++ show vec ++ " elements are of type " ++ show esTy ++ ", but element " ++ show el ++ " has type " ++ show eTy

-- Insert called by indexed element
infer s@(SInsert prevE@(EIdx v prevI) i el) = do
    idxTy <- E.infer i
    when (idxTy /= TInt) $ throwError "Index to insert element in list must be an integer"
    eTy <- E.infer el
    checkMoveNotAllowed eTy el
    (TList esTy) <- E.infer prevE
    when (not $ fitsInto eTy esTy) $ throwError $ "Push: List's " ++ show v ++ " elements are of type " ++ show esTy ++ ", but element " ++ show el ++ " has type " ++ show eTy

-- Insert called by dereferenced element
infer s@(SInsert (EDeref ref) idx el) = do
  idxTy <- E.infer idx
  when (idxTy /= TInt) $ throwError "Insert: index must be an integer"

  refTy <- case ref of
            EVar v -> E.peekVarType v
            _ -> E.infer ref

  esTy <- case refTy of
    TMutRef (TList ty) -> return ty
    TRef _ -> throwError $ "Insert: cannot modify through immutable reference " ++ show ref
    _ -> throwError $ "Insert: " ++ show ref ++ " is not a reference to a list"

  elTy <- E.infer el
  checkMoveNotAllowed elTy el
  unless (fitsInto elTy esTy) $
    throwError $ "Insert: list element type is " ++ show esTy ++
                ", but inserted value has type " ++ show elTy


-- Set value at index cases --
-- These take a list of indices, rather than curry the indexed values of the list, as was done previously --

-- Set called on variable
infer s@(SSetIdx (EVar vec) idxs el) = do
    (VI tp live _ _, mut) <- lookupVarT vec
    when (not live) $ throwError $ "List " ++ show vec ++ " was moved"
    (TList esTy) <- case tp of
                      s@(TList _) -> return s
                      (TRef t) -> throwError $ "Set: Cannot borrow " ++ show vec ++ " as mutable, as it is behind a & reference"
                      (TMutRef t) -> maxUnwrapType t
                      _ -> throwError $ "Cannot use the set function for arrays on non-arrays " ++ show vec
    let exps = [e | IndexList e <- idxs]
    idxsTy <- mapM E.infer exps
    when (any (\idxTy -> idxTy /= TInt) idxsTy) $ throwError "Index to insert element at must be an integer"
    when (mut == Imm) $ throwError $ "List " ++ show vec ++ " is immutable"
    eTy <- E.infer el
    realUnwrappedType <- 
      reconcileListLike eTy esTy ("Set: List's " ++ show vec ++ " elements are of type " ++ show esTy ++ ", but element " ++ show el ++ " has type " ++ show eTy)

    checkMoveNotAllowed realUnwrappedType el
    unwrappedType <- unwrapListType (TList esTy) (length idxs)
    when (not $ fitsInto realUnwrappedType unwrappedType) $
      throwError $ "Set: List's " ++ show vec ++ " elements are of type " ++ show esTy ++ ", but element " ++ show el ++ " has type " ++ show eTy

-- Set called on dereferenced element
infer s@(SSetIdx d@(EDeref ref) idxs el) = do
  refTy <- case ref of
            EVar e -> E.peekVarType e
            _ -> E.infer ref
  (innerListTy, esTy) <- case refTy of
      TMutRef t@(TList es) -> return (t, es)
      TRef _ -> throwError $ "Set: cannot modify through immutable reference " ++ show ref
      _ -> throwError $ "Set: " ++ show ref ++ " is not a reference to a list"
  
  let idxEs = [ e | IndexList e <- idxs ]
  mapM_ (\idx -> do ixTy <- E.infer idx
                    when (ixTy /= TInt) $ throwError "Set: every index must have type int")
        idxEs

  eTy <- E.infer el
  realUnwrappedType <- 
    reconcileListLike eTy esTy ("Set: List's " ++ show ref ++ " elements are of type " ++ show esTy ++ ", but element " ++ show el ++ " has type " ++ show eTy)

  checkMoveNotAllowed realUnwrappedType el
  wantedTy <- unwrapListType innerListTy (length idxs)
  when (not $ fitsInto realUnwrappedType wantedTy) $ 
    throwError $ "Set: element type mismatch. List has " ++ show wantedTy ++ ", expression has " ++ show realUnwrappedType

-- SExp - expression that has its value dropped
infer (SExp e) = do
    E.infer e
    return ()

-- SAssDeref - mutates a value under a mutable reference
infer (SAssDeref e v) = do
  eT <- case e of
          EVar v -> E.peekVarType v
          _ -> E.infer e
  newValT <- E.infer v
  case eT of
    (TRef _) -> throwError "Cannot mutate value under immutable reference"
    (TMutRef rT) -> when (rT /= newValT) $ throwError $ "Value under reference has type " ++ show rT ++ ", but expression " ++ show v ++ "has type " ++ show newValT
    _ -> throwError $ show e ++ " is not a mutable reference (&mut)"

-- For cases where an empty list is provided
-- Given the type that an expression produced and the type we expected,
-- try to "match" nested list layers. If both are lists with the same depth, propagate the inner
-- element type, replacing the unknown type with the concrete expectation.
-- If depths differ, raise the supplied error message.
-- If not a list type is expected, simply return the actual type. 
reconcileListLike :: Type -> Type -> String -> TC Type
reconcileListLike eTy esTy error =
  case (eTy, esTy) of
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
                      throwError error
    (a, e) -> return a

-- Peel nested List layers until something that isn’t a list, counting how many we removed.
unwrapListTypeUntilEnd :: Type -> Int -> TC (Type, Int)
unwrapListTypeUntilEnd (TList listT) n = unwrapListTypeUntilEnd listT (n+1)
unwrapListTypeUntilEnd something n = return (something, n)


-- Re-wrap a type in n List layers
wrapTypeBack :: Type -> Int -> TC Type
wrapTypeBack t 0 = return t
wrapTypeBack t n = wrapTypeBack (TList t) (n-1)

-- Peel nested List layers for a given number of times
unwrapListType :: Type -> Int -> TC Type
unwrapListType something 0 = return something
unwrapListType (TList listT) idxsLeft = unwrapListType listT (idxsLeft - 1)

-- Sequentially type-check a list of statements
inferAndPass :: [Stmt] -> TC ()
inferAndPass [] = return ()
inferAndPass stmts = do
    let stmt = head stmts
    infer stmt
    inferAndPass (tail stmts)
