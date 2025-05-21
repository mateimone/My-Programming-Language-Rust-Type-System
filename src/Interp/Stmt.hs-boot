module Interp.Stmt where

import Lang.Abs (Stmt)
import Env      (Eval)

interp :: Stmt -> Eval ()