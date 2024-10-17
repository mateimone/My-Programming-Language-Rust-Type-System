module Value where

import Lang.Abs ( Exp
                , Ident
                , Type )

data Value 
    = VInt Integer
    | VBool Bool
  deriving (Show, Eq)

data Closure = Fun Ident Exp
  deriving (Show, Eq)

data TClosure = TFun Type Type
  deriving (Show, Eq)
