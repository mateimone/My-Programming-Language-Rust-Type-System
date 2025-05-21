module TypeCheckTests where

import Test.Hspec

import Run ( infertype )
import Lang.Abs ( Type(..) )

import Data.Either ( isLeft )

tcTest :: String -> Type -> Spec
tcTest input expected =
   it (input ++ " should type check to " ++ show expected) $ do
         res <- infertype input
         res `shouldBe` Right expected

tcErrorTest :: String -> Spec
tcErrorTest input =
   it (input ++ " should not type check") $ do
         res <- infertype input
         res `shouldSatisfy` isLeft

test :: IO ()
test = hspec $ do
   describe "typeChecker: good weather tests" $ do
      -- tcTest "let x = 2 in x + 3" TInt
      tcTest "True && False" TBool

      tcTest "val x = 3; x * x" TInt

      tcTest "fun isZero (x : int) -> bool = {x == 0} isZero(42)" TBool

   describe "typeChecker: bad weather tests" $ do
      -- tcErrorTest "let x = 2 in x + True"
      tcErrorTest "True && 3"
      tcErrorTest "fun increment (x : int) -> int = {x + 1} val b = True; increment(b)"

