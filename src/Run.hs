module Run where

import Evaluator

import Lang.Abs ( Type
                , Program )
import TypeCheck.Prog ( infer )

import Value ( Value )
import Interp.Prog ( interp )

infertype :: String -> IO (Result Type)
infertype = evaluateTc (infer, "Type") 

run :: String -> IO (Result Value)
run input = do
    t <- infertype input
    case t of
        Left err -> return (Left err)
        Right _  -> evaluateEv (interp, "Runtime") input 


-- infertype :: String -> IO (Result Type)
-- infertype = evaluate "Type" infer

-- run :: String -> IO (Result Value)
-- run input = do
--     tRes <- infertype input
--     case tRes of
--         Left err -> return (Left err)
--         Right _  -> evaluate "Runtime" interp input