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
    | VList Addr  -- address in heap
    | VRef Addr   -- address in store
    | VMutRef Addr -- address in store
  deriving (Show, Eq)

isCopy :: Type -> Bool
isCopy TInt = True
isCopy TBool = True
isCopy (TRef t) = True
isCopy (TMutRef t) = True
isCopy _ = False -- TLight, TList

fitsInto :: Type -> Type -> Bool
fitsInto  a b 
  | a == b = True
fitsInto (TMutRef t1) (TRef t2) = fitsInto t1 t2
fitsInto (TRef t1) (TRef t2) = fitsInto t1 t2
fitsInto (TMutRef t1) (TMutRef t2) = fitsInto t1 t2
fitsInto _ _ = False

data VarInfo = VI
  { ty :: Type
  , copyFlag :: Bool
  , live :: Bool
  , immutableBorrows :: Int
  , mutableBorrows :: Int
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
