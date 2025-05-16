module Interp.Stmt where

import Evaluator

import Lang.Abs ( Stmt(..) )

import Env
import Value ( Value (..)
             , Closure( Fun )
             , Mutability( Imm, Mut ) )

import qualified Interp.Expr as E
import Lang.Abs (Ident)

-- STATEMENT INTERPRETER -------------------------------------------------------------

-- type Result a = Either String a
-- type Env a = Map.Map Ident (a, Mutability)
interp :: Stmt -> (Env Value, Env Closure) -> Result (Env Value, Env Closure)

-- Supports redeclaration of variables, be they mutable or immutable
interp (SLet x e) env@(vars, funs) = do
    val <- E.interp e env
    return (bind x (val, Imm) vars, funs)

interp (SLetAnn x ty e) env@(vars, funs) = interp (SLet x e) env

interp (SLetM x e) env@(vars, funs) = do
    val <- E.interp e env
    return (bind x (val, Mut) vars, funs)

interp (SLetMAnn x ty e) env@(vars, funs) = interp (SLetM x e) env

interp (SFun f x _ e) env@(vars, funs) = return (vars, bind f (Fun x e, Imm) funs)

-- Currently cannot use functions as first class values
interp (SAss x e) env@(vars, funs) = -- case x of 
    do
        -- is this optimization? find more types at once for efficiency
        mut <- findE x env
        val <- E.interp e env
        case mut of
            (Imm, _) -> throw "An immutable variable may not have its value changed."
            (Mut, Left _) -> return (bind x (val, Mut) vars, funs)
            (Mut, Right _) -> undefined -- return (vars, bind x (val, Mut) funs)

-- interp (SIf cond sts e) env@(vars, funs) = 
--     do
--         vc <- E.interp cond env
--         case vc of
--             VBool True -> do
--                 e <- E.interp e env
                
                

interpAndPass :: [Stmt] -> (Env Value, Env Closure) -> Result (Env Value, Env Closure)
interpAndPass stms env = do
    let stm = head stms
    env <- interp stm env
    interpAndPass (tail stms) env


findE :: Ident -> (Env Value, Env Closure) -> Result (Mutability, Either (Env Value) (Env Closure))
findE x (vars, funs) = do
    case (find x vars, find x funs) of
        (Just (_, mut), Nothing) -> return (mut, Left vars)
        (Nothing, Just(_, mut)) -> return (mut, Right funs)
        _ -> Left ("Variable " ++ show x ++ " not found")