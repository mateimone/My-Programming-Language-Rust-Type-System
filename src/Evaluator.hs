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
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad

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
            ch <- newChan
            _ <- forkIO $ forever (readChan ch >>= putStrLn)
            let startEnv = empty ch
            resEnv <- runEval startEnv (eval prog')
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
            ch <- newChan
            _ <- forkIO $ forever (readChan ch >>= putStrLn)
            let startEnv = empty ch
            resEnv <- runTC startEnv (eval prog')
            let res = fmap fst resEnv
            let c = case res of
                        Left err' -> throw (errDesc ++ " error: " ++ err')
                        right -> right
            return c
