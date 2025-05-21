module InterpTests where

import Test.Hspec

import Run ( run )
import Value

interpTest :: String -> Value -> Spec
interpTest input expected =
    it (input ++ " should be " ++ show expected) $ do
            result <- run input
            result `shouldBe` Right expected

test :: IO ()
test = hspec $ do
    describe "Interpreter: good weather tests" $ do
       -- interpTest "let x = 2 in x + 3" (VInt 5)
       interpTest "True && False" (VBool False)

       interpTest "val x = 3; x * x" (VInt 9)
       interpTest "fun square (x : int) -> int = {x * x} square(3)" (VInt 9)

    describe "Interpreter: order of operation" $ do
       it "multiplication should come before addition" $ do
              result <- run "2 + 3 * 4"
              result `shouldBe` Right (VInt 14)
              result2 <- run "4 * 3 + 2"
              result2 `shouldBe` result

       it "brackets should have the highest priority" $ do
              result <- run "(2 + 3) * 4"
              result `shouldBe` Right (VInt 20)

