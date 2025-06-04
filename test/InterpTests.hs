module InterpTests where

import Test.Hspec

import Run ( run )
import Value
import Data.Either ( isLeft )

interpTest :: String -> Value -> Spec
interpTest input expected =
    it (input ++ " should be " ++ show expected) $ do
            result <- run input
            result `shouldBe` Right expected

interpErrorTest :: String -> Spec
interpErrorTest input =
    it (input ++ " should not interpret ") $ do
         res <- run input
         res `shouldSatisfy` isLeft

test :: IO ()
test = hspec $ do
   describe "Interpreter: good weather tests" $ do
   -- interpTest "let x = 2 in x + 3" (VInt 5)
      interpTest "return (True && False)" (VBool False)

      interpTest "val x = 3; return (x * x)" (VInt 9)
      interpTest "fun square (x : int) -> int = {return (x * x)} return square(3)" (VInt 9)
      interpTest "val mut x = 2; fun square (x : int) -> int = {return (x * x)} val z = square(x); return x" (VInt 2)

   describe "Interpreter: order of operation" $ do
      it "multiplication should come before addition" $ do
            result <- run "return (2 + 3 * 4)"
            result `shouldBe` Right (VInt 14)
            result2 <- run "return (4 * 3 + 2)"
            result2 `shouldBe` result

      it "brackets should have the highest priority" $ do
            result <- run "return ((2 + 3) * 4)"
            result `shouldBe` Right (VInt 20)

   describe "Interpreter: Primitive copy tests" $ do
      interpTest  ("val a = 1;" ++
               "val b = a;" ++
               "val c = a;" ++
               "fun test(x:int) -> int = {return x}" ++ 
               "val a = test(a);" ++
               "return a"
            ) (VInt 1)

   describe "typeChecker: list tests" $ do
      interpTest  ("val list = vec![1,2,3];" ++
               "val a = list[0];" ++ 
               "val b = list[1];" ++
               "return list[2]"
            ) (VInt 3)

      interpTest  ("val mut list = vec![1,2,3];" ++
               "val a = list[0];" ++ 
               "val b = list[1];" ++
               "list[2] = 4;" ++
               "return list[2]"
            ) (VInt 4)

      interpTest  ("val mut list = vec![1,2,3];" ++
               "val a = list[0];" ++ 
               "val b = list[0];" ++
               "list.insert(3, 4);" ++
               "list.insert(0, 5);" ++
               "return ((list[4] == 4) && (list[0] == 5))"
            ) (VBool True)

      interpTest  ("val mut list = vec![1,2,3];" ++
               "val a = list[0];" ++ 
               "val b = list[0];" ++
               "list.push(4);" ++
               "return list[3]"
            ) (VInt 4)

      interpTest  ("val mut list = vec![1,2,3];" ++
               "val a = list[0];" ++ 
               "val b = list[0];" ++
               "val p = list.remove(1);" ++
               "return list[1]"
            ) (VInt 3)

      interpTest  ("val mut list = vec![Green];" ++
                    "val a = list.remove(0);" ++ 
                    "return a"
               ) (VLight Green)

      interpTest  ("val mut list = vec![Green];" ++
                   "return list.remove(0)" 
               ) (VLight Green)

      interpTest  ("val mut list = vec![Green, Red];" ++
                   "return ((list.remove(0) == Green) && (list.remove(0) == Red))"
               ) (VBool True)

      interpTest  ("val list = vec![vec![vec![1,2,3]]];" ++ 
               "val a = list[0][0][0];" ++ 
               "return ((a == 1) && (list[0][0][2] == 3))"
            ) (VBool True)
      
      interpTest  ("val list:List<List<List<int>>> = vec![vec![vec![1,2,3]]];" ++ 
               "val a = list[0][0][0];" ++ 
               "return ((a == 1) && (list[0][0][2] == 3))"
            ) (VBool True)

      interpTest  ("val mut list = vec![vec![vec![1,2,3]]];" ++ 
               "val a = list[0][0][0];" ++ 
               "val x = (list[0][0]).remove(1);" ++ 
               "return ((a == 1) && (x == 2) && ((list[0][0]).remove(1) == 3))"
            ) (VBool True)

      interpTest  ("val mut list = vec![vec![vec![1,2,3]]];" ++ 
               "val a = (list[0]).remove(0);" ++ 
               "return (a[1] == 2)"
            ) (VBool True)

      interpTest  ("val mut list = vec![vec![vec![1,2,3]]];" ++ 
               "val a = (list[0][0]).remove(1);" ++ 
               "return (a == 2)"
            ) (VBool True)

      interpTest  ("val mut list = vec![vec![vec![1,2,3]]];" ++ 
               "(list[0]).push(vec![6, 7, 8]);" ++ 
               "return (list[0][1][2] == 8)"
            ) (VBool True)

      interpTest  ("val mut list = vec![vec![vec![1,2,3]]];" ++ 
               "(list[0][0]).insert(0, 4);" ++ 
               "return ((list[0][0][0] == 4) && (list[0][0][1] == 1) && (list[0][0][2] == 2) && (list[0][0][3] == 3))"
            ) (VBool True)
         
      -- interpTest  ("val mut list = vec![vec![vec![1,2,3]]];" ++ 
      --          "val a = (list.remove(0).remove(0))[0];" ++ 
      --          "val b = a;" ++ 
      --          "return b"
      --       ) (VInt 1)

      interpErrorTest  ("val mut list = vec![vec![vec![1,2,3]]];" ++ 
               "val a = list[0][0];" ++ 
               "return -1"
            ) 
      
      it "Out of bounds" $ do
         a <- run  ("val list = vec![1];" ++
                     "return list[1]"
                  ) 
         a `shouldSatisfy` isLeft

         b <- run ("val list = vec![1];" ++
                    "list[0] = 3;" ++ 
                    "return 0"
               )
         b `shouldSatisfy` isLeft

         c <- run ("val mut list = vec![1];" ++
                    "val a = list.remove(0);" ++ 
                    "return list[0]"
               )
         c `shouldSatisfy` isLeft

      interpErrorTest  ("val mut list = vec![1];" ++
                    "val a = list.remove(1);" ++ 
                    "return 0"
               )

      interpErrorTest  ("val mut list = vec![1];" ++
                    "list[1] = 3;" ++ 
                    "return 0"
               )

      interpErrorTest  ("val mut list = vec![1,2,3];" ++
               "val a = list[0];" ++ 
               "val b = list[0];" ++
               "list.insert(4, 4);" ++
               "return list[3]"
               )

   describe "Interpreter: Light tests (for moving)" $ do
      interpTest  ("val mut lamp1 = Green;" ++ 
               "val mut lamp2 = Yellow;" ++
               "ass lamp2 = lamp1;" ++
               "ass lamp1 = Red;" ++
               "fun test(l:Light) -> Light = {return Green}" ++
               "return lamp2"
            ) (VLight Green)

      interpTest  ("val mut lamp1 = Green;" ++ 
               "val mut lamp2 = lamp1;" ++
               "ass lamp1 = Red;" ++
               "fun test(l:Light) -> Light = {return Green}" ++
               "val u1 = test(lamp1);" ++ 
               "return lamp2"
            ) (VLight Green)

      interpTest  ("val lamp1 = Green;" ++ 
               "val lamp2 = lamp1;" ++
               "val lamp1 = Red;" ++
               "fun test(l:Light) -> Light = {return Green}" ++
               "val u1 = test(lamp1);" ++ 
               "return lamp2"
            ) (VLight Green)

      interpTest  ("val lamp1 = Green;" ++ 
               "val lamp2 = lamp1;" ++
               "val lamp1 = Red;" ++
               "fun test(l:Light) -> Light = {return Green}" ++
               "val u1 = test(lamp1);" ++ 
               "return lamp2"
            ) (VLight Green)

      interpTest  ("val lamp1 = Green;" ++ 
               "val mut lamp2 = lamp1;" ++
               "val lamp1 = Yellow;" ++ 
               "ass lamp2 = lamp1;" ++
               "return lamp2"
            ) (VLight Yellow)

      interpTest  ("val mut lamp1 = Green;" ++ 
               "fun test(mut l:Light) -> Light = {return l}" ++
               "val lamp1 = test(lamp1);" ++ 
               "return lamp1"
            ) (VLight Green)

      interpTest  ("val mut lamp1 = Green;" ++ 
               "val mut lamp2 = Yellow;" ++
               "ass lamp2 = lamp1;" ++
               "ass lamp1 = Red;" ++
               "fun test(l:Light) -> Light = {return Green}" ++
               "val u1 = test(lamp1);" ++ 
               "return lamp2"
            ) (VLight Green)

      interpTest  ("val mut lamp1 = Green;" ++ 
               "val mut lamp2 = lamp1;" ++
               "ass lamp1 = Red;" ++
               "fun test(l:Light) -> Light = {return Green}" ++
               "val u1 = test(lamp1);" ++ 
               "return lamp2"
            ) (VLight Green)

      interpTest  ("val lamp1 = Green;" ++ 
               "val lamp2 = lamp1;" ++
               "val lamp1 = Red;" ++
               "fun test(l:Light) -> Light = {return Green}" ++
               "val u1 = test(lamp1);" ++ 
               "return lamp2"
            ) (VLight Green)

      interpTest  ("val lamp1 = Green;" ++ 
               "val mut lamp2 = lamp1;" ++
               "val lamp1 = Yellow;" ++ 
               "ass lamp2 = lamp1;" ++
               "return lamp2"
            ) (VLight Yellow)

      interpTest  ("val lamp1 = Red;" ++ 
               "val mut lamp2 = lamp1;" ++
               "ass lamp2 = Green;" ++ 
               "val lamp3 = lamp2;" ++ 
               "return lamp3"
            ) (VLight Green)

      interpTest  ("fun immutable(l:Light) -> Light = {return Red}" ++
               "fun mutable(mut l:Light) -> Light = {return Red}" ++ 
               "val imm_light = Red;" ++ 
               "val z = immutable(imm_light);" ++
               "val imm_light = Red;" ++ 
               "val zz = mutable(imm_light);" ++ 
               "val mut mut_light = Yellow;" ++
               "val zzz = immutable(mut_light);" ++ 
               "val mut mut_light = Yellow;" ++ 
               "val zzzz = mutable(mut_light);" ++ 
               "return zzzz"
            ) (VLight Red)

   

