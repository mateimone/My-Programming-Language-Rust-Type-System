module TypeCheck.Stmt where

import Evaluator

import Env

import Value ( TClosure( TFun )
             , Mutability( Imm, Mut ) )

import Lang.Abs ( Stmt(..)
                , Type 
                , Ident)

import qualified TypeCheck.Expr as E

-- STATEMENT TYPE CHECKER ------------------------------------------------------------

infer :: Stmt -> (Env Type, Env TClosure) -> Result (Env Type, Env TClosure)

infer (SLet x e) env@(vars, funs) = do
    t <- E.infer e env
    return (bind x (t, Imm) vars, funs)

infer (SLetAnn x ty e) env@(vars,funs) = do
  tExpr <- E.infer e env
  if tExpr == ty
     then return (bind x (ty, Imm) vars , funs)
     else throw $
       "Type mismatch for " ++ show x ++
       ": annotation says " ++ show ty ++
       " but expression has type " ++ show tExpr

infer (SLetM x e) env@(vars, funs) = do
    val <- E.infer e env
    return (bind x (val, Mut) vars, funs)

infer (SLetMAnn x ty e) env@(vars,funs) = do
  tExpr <- E.infer e env
  if tExpr == ty
     then return (bind x (ty, Mut) vars , funs)
     else throw $
       "Type mismatch for " ++ show x ++
       ": annotation says " ++ show ty ++
       " but expression has type " ++ show tExpr

-- Currently function parameters can only be immutable!
infer (SFun f x t e) (vars, funs) = do
    ret <- E.infer e (bind x (t, Imm) vars, funs)
    return (vars, bind f (TFun t ret, Imm) funs)

-- Currently cannot use functions as first class values
infer (SAss x e) env@(vars, funs) = do
  case find x vars of
    Nothing -> throw $ "Assignment to unbound variable " ++ show x
    Just (tVar, mut) ->
      case mut of
        Imm -> throw $ "Cannot assign to immutable variable " ++ show x
        Mut -> do
          tExpr <- E.infer e env
          if tExpr == tVar
            then pure (vars, funs)  -- environment unchanged, just check the assignment
            else throw $ "Type mismatch in assignment to " ++ show x
                      ++ ": variable has type " ++ show tVar
                      ++ ", but expression has type " ++ show tExpr
        
-- findE :: Ident -> (Env Type, Env TClosure) -> Result (Mutability, Either (Env Type) (Env TClosure))
-- findE x (vars, funs) = do
--     case (find x vars, find x funs) of
--         (Just (_, mut), Nothing) -> return (mut, Left vars)
--         (Nothing, Just(_, mut)) -> return (mut, Right funs)
--         _ -> Left ("Variable " ++ show x ++ " not found")
