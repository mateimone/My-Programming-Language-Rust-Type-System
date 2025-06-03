module Value where

import Lang.Abs ( Exp
                , Ident
                , Type
                , Stmt )
import Lang.Abs (Type(..))


newtype Addr = Addr Int 
    deriving (Eq, Ord, Show)

data Slot v
  = Prim v  -- primitive values
  -- | Ref Addr    -- for example for: let a = &1;
  -- | Borrowed ? Addr  
  | Moved       -- after a move 
  deriving (Eq, Show)

data Value 
    = VInt Integer
    | VBool Bool
    | VUnit 
    | VLight Color
    | VList Addr
  deriving (Show, Eq)

isCopy :: Type -> Bool
isCopy TInt = True
isCopy TBool = True
isCopy TUnit = False
isCopy _ = False -- TLight, TList

data VarInfo = VI
  { ty :: Type
  , copyFlag :: Bool
  , live :: Bool
  }
  deriving (Show, Eq)

data Object
    = OList [Slot Value]
  deriving (Show, Eq)

data Color = Red | Yellow | Green
  deriving (Show, Eq)

data Closure = Fun [(Ident, Mutability)] [Stmt] Exp
  deriving (Show, Eq)

data TClosure = TFun [(Type, Mutability)] Type
  deriving (Show, Eq)

data Mutability = Imm | Mut
  deriving (Show, Eq)
