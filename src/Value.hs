module Value where

import Lang.Abs ( Exp
                , Ident
                , Type
                , Stmt )

data Value 
    = VInt Integer
    | VBool Bool
    | VUnit 
  deriving (Show, Eq)

data Closure = Fun [(Ident,Mutability)] [Stmt] Exp
  deriving (Show, Eq)

data TClosure = TFun [(Type,Mutability)] Type
  deriving (Show, Eq)

data Mutability = Imm | Mut
  deriving (Show, Eq)
