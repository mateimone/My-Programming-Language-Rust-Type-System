module Value where

import Lang.Abs ( Exp
                , Ident
                , Type
                , Stmt
                , Light )
import Lang.Abs (Type(..))


newtype Addr = Addr Int 
    deriving (Eq, Ord, Show)

data Value 
    = VInt Integer
    | VBool Bool
    | VUnit 
    | VLight Light
    | VList Addr  -- address in heap
    | VRef Addr   -- address in store
    | VMutRef Addr -- address in store
  deriving (Show, Eq)

isCopy :: Type -> Bool
isCopy TInt = True
isCopy TBool = True
isCopy (TRef t) = True
isCopy (TMutRef t) = True
isCopy _ = False

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
    = OList [Value]
  deriving (Show, Eq)

data Closure = Fun [(Ident, Mutability)] [Stmt] Exp
  deriving (Show, Eq)

data TClosure = TFun [(Type, Mutability)] Type
  deriving (Show, Eq)

data Mutability = Imm | Mut
  deriving (Show, Eq)
