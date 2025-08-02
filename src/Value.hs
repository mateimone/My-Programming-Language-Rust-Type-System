module Value where

import Lang.Abs ( Exp
                , Ident
                , Type
                , Stmt
                , Light )
import Lang.Abs (Type(..))

-- Address type for addresses in heap and reference store
newtype Addr = Addr Int 
    deriving (Eq, Ord, Show)

-- Values
data Value 
    = VInt Integer
    | VBool Bool
    | VUnit 
    | VLight Light
    | VList Addr  -- address in heap
    | VRef Addr   -- address in store
    | VMutRef Addr -- address in store
  deriving (Show, Eq)

-- Returns whether the type of a variable has cheap-copying
isCopy :: Type -> Bool
isCopy TInt = True
isCopy TBool = True
isCopy (TRef t) = True
isCopy (TMutRef t) = False
isCopy _ = False

-- Returns whether the type of an actual value fits into the type of an expected value
fitsInto :: Type -> Type -> Bool
fitsInto  a b 
  | a == b = True
fitsInto (TMutRef t1) (TRef t2) = fitsInto t1 t2
fitsInto (TRef t1) (TRef t2) = fitsInto t1 t2
fitsInto (TMutRef t1) (TMutRef t2) = fitsInto t1 t2
fitsInto _ _ = False

-- Variable information required by typechecker
data VarInfo = VI
  { ty :: Type
  , live :: Bool
  , immutableBorrows :: Int
  , mutableBorrows :: Int
  }
  deriving (Show, Eq)

-- Objects that can live on the heap
data Object
    = OList [Value]
  deriving (Show, Eq)

data Closure = Fun [(Ident, Mutability)] [Stmt] Exp
  deriving (Show, Eq)

data TClosure = TFun [(Type, Mutability)] Type
  deriving (Show, Eq)

-- Mutability, used for mutable/immutable variables and references
data Mutability = Imm | Mut
  deriving (Show, Eq)
