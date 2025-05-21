module Evaluator where

import Env ( Env
           , empty
           , Eval
           , runEval 
           , TC
           , runTC)

import Lang.Abs ( Program )

import Lang.Par ( myLexer
                , pProgram )

import Lang.ErrM 

import Value

type Result a = Either String a

throw :: String -> Result a
throw = Left

type Evaluator a b = Program -> Env a b -> Result a

stripEnv :: IO (Either e (a, env)) -> IO (Either e a)
stripEnv = fmap (fmap fst)

evaluateEv :: (Program -> Eval a, String) -> String -> IO (Result a)
evaluateEv (eval, errDesc) input = do
    let prog = pProgram (myLexer input)
    case prog of
        Bad err -> return (Left (errDesc ++ " error: " ++ err))
        Ok prog' -> do
            resEnv <- runEval empty (eval prog')
            let res = fmap fst resEnv
            let c = case res of
                        Left err' -> throw (errDesc ++ " error: " ++ err')
                        right -> right
            return c

evaluateTc :: (Program -> TC a, String) -> String -> IO (Result a)
evaluateTc (eval, errDesc) input = do
    let prog = pProgram (myLexer input)
    case prog of
        Bad err -> return (Left (errDesc ++ " error: " ++ err))
        Ok prog' -> do
            resEnv <- runTC empty (eval prog')
            let res = fmap fst resEnv
            let c = case res of
                        Left err' -> throw (errDesc ++ " error: " ++ err')
                        right -> right
            return c
