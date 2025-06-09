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
      tcTest "return (True && False)" TBool

      tcTest "val x = 3; return (x * x)" TInt

      tcTest "fun isZero (x : int) -> bool = {return x == 0} return isZero(42)" TBool

   describe "typeChecker: bad weather tests" $ do
      -- tcErrorTest "let x = 2 in x + True"
      tcErrorTest "True && 3"
      tcErrorTest "fun increment (x : int) -> int = {return x + 1} val b = True; return increment(b)"

   describe "typeChecker: dereference tests (immutable references)" $ do
      tcTest  ("val a = &(Green);" ++ 
               "val b = &a;" ++ 
               "val c = *b;" ++ 
               "return (c == &(Green)) && (b == &(&(Green)))"
            ) TBool
      
      tcTest  ("val a = Green;" ++ 
               "val b = &(&a);" ++ 
               "val c = *b;" ++ 
               "return (c == &(Green)) && (b == &(&(Green)))"
            ) TBool

      tcTest  ("val a = Green;" ++ 
               "val b = &(&(&a));" ++ 
               "val c = *(*b);" ++ 
               "return (c == &(Green)) && (b == &(&(&(Green))))"
            ) TBool

      tcTest  ("val a = Green;" ++ 
               "val b = &(&a);" ++ 
               "val c = *b;" ++ 
               "return (c == *(*(&(&(&(Green))))))"
            ) TBool

      tcErrorTest  ("val a = Green;" ++ 
                    "val b = &a;" ++ 
                    "*b = Red;" ++ 
                    "return 0"
               )

      tcErrorTest  ("val a = Green;" ++ 
                    "val b = &a;" ++ 
                    "val c = *a;" ++ 
                    "return 0"
               )

      tcErrorTest  ("val a = Green;" ++ 
                    "val b = &a;" ++ 
                    "val c = *b;" ++ 
                    "return 0"
               )

      tcErrorTest  ("val a = *Green;" ++ 
                    "return 0"
               )

      tcErrorTest  ("val a = *(&(Green));" ++ 
                    "return 0"
               ) 
   
   -- fitsInto done for
   -- vectors
   -- variable assignment
   -- function application

   describe "typeChecker: mutable reference and dereferences tests" $ do
      tcTest  ("val mut a = &(Green);" ++ 
               "val b = &mut a;" ++ 
               "*b = &(Yellow);" ++
               "return (a == &(Yellow))"
            ) TBool

      tcTest  ("val mut a = &mut(Green);" ++ 
               "val b = &mut a;" ++ 
               "*b = &mut (Yellow);" ++
               "return (a == &mut(Yellow))"
            ) TBool

      tcTest  ("val mut a = &mut(vec![1]);" ++ 
               "val b = &mut a;" ++ 
               "val c = (*b)[0];" ++
               "return (c == 1)"
            ) TBool

      tcTest  ("val mut a = &(&(vec![1]));" ++ 
               "val b = &mut (&mut a);" ++ 
               "val c = b[0];" ++
               "return (c == 1)"
            ) TBool

      tcTest  ("val mut a = &mut(vec![1]);" ++ 
               "val b = &mut a;" ++ 
               "val c = (*b)[0];" ++
               "return (c == 1)"
            ) TBool

      tcTest  ("val mut a = &(vec![Yellow]);" ++
               "val b = &mut a;" ++ 
               "fun test(x: &mut &List<Light>) -> unit = { *x = (&(vec![Red])); return void }" ++
               "test(b);" ++
               "val c = &(b[0]);" ++
               "return (c == &(Red)) "
            ) TBool

      tcTest  ("val mut a = vec![1];" ++ 
               "val b = &mut a;" ++ 
               "*b = (vec![2]);" ++
               "return (a[0])"
            ) TInt

      tcTest  ("val mut a = vec![Yellow];" ++ 
               "val b = &mut a;" ++ 
               "*b = (vec![Red]);" ++
               "return (&((*(&b))[0]))"
            ) (TRef TLight)

      tcTest  ("fun test(x: &mut (&mut List<&int>)) -> int = { return (*(x[0])) }" ++
               "return (test(&mut (&mut (vec![&(1)]))) == 1)"
            ) TBool

      tcTest  ("fun test(x: &mut (&mut List<&int>)) -> int = { return (*((*x)[0])) }" ++
               "return (test(&mut (&mut (vec![&(1)]))) == 1)"
            ) TBool

      tcTest  ("val mut a = vec![Yellow];" ++
               "val b = &mut a;" ++
               "(*b)[0] = (Red);" ++ 
               "return (&((&b)[0]))"
            ) (TRef TLight)

      tcTest  ("val mut a = vec![Yellow];" ++
               "val b = &mut (&mut a);" ++
               "(*(*b))[0] = (Red);" ++ 
               "return (  &(  (*(*(&b)))[0]  )  == &(Red))"
            ) TBool

      tcTest  ("val mut a = Red;" ++
               "val b = &mut a;" ++
               "fun test(x: &mut Light) -> unit = { *x = (Red); return void }" ++
               "test(b);" ++
               "return 0"
            ) TInt -- can't really test this yet

      tcTest  ("val mut a = vec![Green, Red];" ++
               "val b = &mut a;" ++
               "fun test(x: &mut List<Light>) -> unit = { (*x)[0] = Red; return void }" ++
               "test(b);" ++
               "return 0"
            ) TInt -- can't really test this yet

      tcErrorTest ("val mut a = vec![Yellow];" ++ 
               "val mut b = &mut a;" ++ 
               "val c = &((*(&b))[0]);" ++
               "val d = &mut b;" ++
               "return 0"
            )

      tcErrorTest  ("val mut a = vec![Yellow];" ++ 
               "val mut b = &mut a;" ++ 
               "val c = &((*(&b))[0]);" ++
               "fun test(x: &mut List<Light>) -> unit = { return void }" ++
               "test(b);" ++
               "return 0"
            ) 

      tcErrorTest  ("val mut a = vec![Yellow];" ++
                    "val b = &mut a;" ++
                    "val c = &mut a;"
               )

      tcErrorTest  ("val mut a = vec![Yellow];" ++
                    "val b = &mut a;" ++
                    "val c = &a;"
               )
      
      tcErrorTest  ("val mut a = vec![Yellow];" ++
                    "val b = &a;" ++
                    "val c = &mut a;"
               )
      

   describe "typeChecker: immutable reference tests" $ do
      -- need to test dereferencing!!!

      tcTest  ("val a = Green;" ++ 
               "val b = &a;" ++ 
               "val c = &a;" ++ 
               "return (b == c) && (b == &(Green))"
            ) TBool

      tcTest  ("val a = &(&(vec![vec![3]]));" ++ 
               "val b = &a;" ++ 
               "val c = &a;" ++ 
               "return (b == c) && (b[0][0] == 3)"
            ) TBool

      tcTest  ("val a = True;" ++ 
               "val b = &(&(&a));" ++ 
               "val c = &(&(&a));" ++ 
               "return (b == c)"
            ) TBool

      tcTest  ("val a = vec![vec![3]];" ++ 
               "val b = &a;" ++ 
               "val c = &a;" ++ 
               "return (b == c) && (b[0][0] == 3)"
            ) TBool

      tcTest  ("val a = 10;" ++ 
               "val b = &a;" ++ 
               "val c = &a;" ++ 
               "return (a + b == 20) && (b + c == 20)"
            ) TBool

      tcTest  ("val a = 10;" ++ 
               "val b = &a;" ++ 
               "val c = &(&a);" ++ 
               "return (&a >= b) && (&b >= c) && (c >= &(&a))"
            ) TBool

      tcTest  ("val a = 10;" ++ 
               "val b = &a;" ++ 
               "val c = &(&a);" ++ 
               "return (&a >= b) && (&b >= c) && (c >= &(&a))"
            ) TBool

      tcTest  ("fun test(a:&List<int>) -> int = {return a[1]}" ++ 
               "val a = &(vec![1,2,3]);" ++ 
               "return test(a)"
            ) TInt

      tcTest  ("fun test(i: &(&List<int>)) -> int = {return i[0]}" ++ 
               "val a = Green;" ++
               "val b = &a;" ++
               "val d = &a;" ++
               "val e = &(Red);" ++
               "val o = e;" ++
               "val nnn = &(&(Green));" ++
               "return (test(&(&(vec![1]))) == 1)"
            ) TBool

      tcErrorTest  ("val a = 10;" ++ 
                    "val b = &a;" ++ 
                    "val c = &a;" ++ 
                    "return (a + (&b) == 20) && (b + c == 20)"
                  )

      tcErrorTest  ("val a = 10;" ++ 
                    "val b = &a;" ++ 
                    "return (b >= a)"
                  ) 

      tcErrorTest  ("val a = 10;" ++ 
                    "val b = &(&a);" ++ 
                    "val c = &a;" ++ 
                    "return (b + b == 0) && (c + b == 0)"
                  )

      

   describe "typeChecker: list tests" $ do
      tcTest  ("val mut list: List<List<List<int>>> = vec![vec![vec![]]];" ++
               "ass list = vec![vec![vec![]]];" ++
               "return 0"
            ) TInt

      tcTest  ("val mut list: List<int> = vec![];" ++
                "list.push(1);" ++
                "return list[1]"
            ) TInt

      tcTest  ("val mut list: List<List<int>> = vec![vec![],vec![]];" ++
                "(list[0]).push(1);" ++
                "return list[0][0]"
            ) TInt

      tcTest  ("fun test(x:List<int>) -> int = {return x[0]}" ++
               "return test(vec![1])"
            ) TInt

      tcTest  ("val list = vec![1,2,3];" ++
               "val a = list[0];" ++ 
               "val b = list[1];" ++
               "return list[2]"
            ) TInt

      tcTest  ("val mut list = vec![1,2,3];" ++
               "val a = list[0];" ++ 
               "val b = list[1];" ++
               "list[2] = 4;" ++
               "return list[2]"
            ) TInt

      tcTest  ("val mut list = vec![1,2,3];" ++
               "val a = list[0];" ++ 
               "val b = list[0];" ++
               "list.insert(3, 4);" ++
               "return list[3]"
            ) TInt

      tcTest  ("val mut list = vec![1,2,3];" ++
               "val a = list[0];" ++ 
               "val b = list[0];" ++
               "list.push(4);" ++
               "return list[3]"
            ) TInt

      tcTest  ("val mut list = vec![1,2,3];" ++
               "val a = list[0];" ++ 
               "val b = list[0];" ++
               "val p = list.remove(1);" ++
               "return list[1]"
            ) TInt

      tcTest  ("val mut list = vec![Green];" ++
                    "val a = list.remove(0);" ++ 
                    "return a"
               ) TLight

      tcTest  ("val mut list = vec![Green];" ++
               "return list.remove(0)" 
               ) TLight

      tcTest  ("val mut list = vec![vec![vec![1]]];" ++
               "return list" 
               ) (TList (TList (TList TInt)))

      tcTest  ("val mut list:List<Light> = vec![Green];" ++ 
               "val a:Light = Red;" ++ 
               "list.insert(0, a);" ++ 
               "return 0"
            ) TInt

      tcTest  ("val list = vec![vec![vec![1,2,3]]];" ++ 
               "val a = list[0][0][0];" ++ 
               "return ((a == 1) && (list[0][0][2] == 3))"
            ) TBool
      
      tcTest  ("val list:List<List<List<int>>> = vec![vec![vec![1,2,3]]];" ++ 
               "val a = list[0][0][0];" ++ 
               "return ((a == 1) && (list[0][0][2] == 3))"
            ) TBool

      tcTest  ("val mut list = vec![vec![vec![1,2,3]]];" ++ 
               "val a = list[0][0][0];" ++ 
               "val x = (list[0][0]).remove(1);" ++ 
               "return ((a == 1) && (x == 2) && ((list[0][0]).remove(1) == 3))"
            ) TBool

      tcTest  ("val mut list = vec![vec![vec![1,2,3]]];" ++ 
               "val a = (list[0]).remove(0);" ++ 
               "return (a[1] == 2)"
            ) TBool

      tcTest  ("val mut list = vec![vec![vec![1,2,3]]];" ++ 
               "val a = (list[0][0]).remove(1);" ++ 
               "return (a == 2)"
            ) TBool

      tcTest  ("val mut list = vec![vec![vec![1,2,3]]];" ++ 
               "(list[0]).push(vec![6, 7, 8]);" ++ 
               "return (list[0][1][2] == 8)"
            ) TBool

      tcTest  ("val mut list = vec![vec![vec![1,2,3]]];" ++ 
               "(list[0][0]).insert(0, 4);" ++ 
               "return ((list[0][0][0] == 4) && (list[0][0][1] == 1) && (list[0][0][2] == 2) && (list[0][0][3] == 3))"
            ) TBool
      
      tcTest  ("val mut list = vec![vec![vec![1,2,3]]];" ++ 
               "val a = (list.remove(0))[0][2];" ++ 
               "return (a == 3)"
            ) TBool

      tcErrorTest  "val list = vec![]; return 0"

      tcErrorTest  ("val mut list = vec![vec![vec![1,2,3]]];" ++ 
               "val a = list[0][0];" ++ 
               "return -1"
            ) 

      tcErrorTest  ("val list = vec![1, Green];" ++
                    "return 0"
               )
            
      tcErrorTest  ("val list = vec![1];" ++
                    "list[0] = 3;" ++ 
                    "return 0"
               )

      tcErrorTest  ("val list = vec![1];" ++
                    "list.insert(0, 2);" ++ 
                    "return 0"
               )

      tcErrorTest  ("val list = vec![1];" ++
                    "list.push(2);" ++ 
                    "return 0"
               )

      tcErrorTest  ("val list = vec![1];" ++
                    "list[0] = 3;" ++ 
                    "return 0"
               )

      tcErrorTest  ("val mut list = vec![Green];" ++
                    "val a = list[0];" ++ 
                    "return 0"
               )

      tcErrorTest  ("val mut list = vec![Green];" ++
                    "list.insert(0, 2);" ++ 
                    "return 0"
               )

      tcErrorTest  ("val mut list = vec![Green];" ++
                    "list.push(1);" ++ 
                    "return 0"
               )

      tcErrorTest  ("val mut list:List<Light> = vec![1];" ++
                    "return 0"
               )

      tcErrorTest  ("val mut list = vec![Green];" ++
                    "val a: int = list[0];" ++ 
                    "return 0"
               )
      
      tcErrorTest  ("val list = vec![Green];" ++
                    "return list[0]"
               )

      tcErrorTest  ("val mut list = vec![Green, Red];" ++
                   "fun test(list:List<Light>) -> Light = {val o = list.remove(0); val o = list.remove(0); return Green}" ++
                   "val z = test(list)" ++
                   "return ((list.remove(0) == Green) && (list.remove(0) == Red))"
               )
      
      tcErrorTest  ("val mut list = vec![0, 1];" ++
                   "fun test(list:List<int>) -> Light = {val o = list.remove(0); val o = list.remove(0); return Green}" ++
                   "val z = test(list)" ++
                   "return ((list.remove(0) == 0) && (list.remove(0) == 1))"
               ) 

   describe "typeChecker: primitive copy tests" $ do
      tcTest  ("val a = 1;" ++
               "val b = a;" ++
               "val c = a;" ++
               "fun test(x:int) -> int = {return x}" ++ 
               "val a = test(a);" ++
               "return a"
            ) TInt
   
   describe "typeChecker: light tests (for moving)" $ do
      tcErrorTest ("val lamp1 = Green;" ++ 
             "val lamp2 = Yellow;" ++
             "fun test(lamp1:Light, lamp2:Light) -> bool = {" ++
             "return lamp1 == lamp2" ++
             "}" ++ 
             "val b1 = test(lamp1, lamp2);" ++ 
             "val b2 = test(lamp1, lamp2);" ++ 
             "return b2")
      
      tcErrorTest ("val lamp1 = Green;" ++ 
                   "val lamp2 = lamp1;" ++
                   "val lamp3 = lamp1;" ++ 
                   "return lamp3"
            )

      tcErrorTest  ("val mut lamp2 = Red;" ++
                    "fun test(l:Light) -> Light = {return Green}" ++
                    "val u2 = test(lamp2);" ++ 
                    "return lamp2"
            ) 

      tcTest  ("val mut lamp1 = Green;" ++ 
               "val mut lamp2 = Yellow;" ++
               "ass lamp2 = lamp1;" ++
               "ass lamp1 = Red;" ++
               "fun test(l:Light) -> Light = {return Green}" ++
               "val u1 = test(lamp1);" ++ 
               "val u2 = test(lamp2);" ++ 
               "return u2"
            ) TLight

      tcTest  ("val mut lamp1 = Green;" ++ 
               "val mut lamp2 = lamp1;" ++
               "ass lamp1 = Red;" ++
               "fun test(l:Light) -> Light = {return Green}" ++
               "val u1 = test(lamp1);" ++ 
               "val u2 = test(lamp2);" ++ 
               "return u2"
            ) TLight

      tcTest  ("val lamp1 = Green;" ++ 
               "val lamp2 = lamp1;" ++
               "val lamp1 = Red;" ++
               "fun test(l:Light) -> Light = {return Green}" ++
               "val u1 = test(lamp1);" ++ 
               "val u2 = test(lamp2);" ++ 
               "return u2"
            ) TLight

      tcTest  ("val lamp1 = Green;" ++ 
               "val mut lamp2 = lamp1;" ++
               "val lamp1 = Yellow;" ++ 
               "ass lamp2 = lamp1;" ++
               "return lamp2"
            ) TLight

      tcTest  ("val mut lamp1 = Green;" ++ 
               "fun test(mut l:Light) -> Light = {return l}" ++
               "val lamp1 = test(lamp1);" ++ 
               "return lamp1"
            ) TLight

      tcErrorTest ("val lamp1 = Green;" ++ 
             "val lamp2 = Yellow;" ++
             "fun test(lamp1:Light, lamp2:Light) -> bool = {" ++
             "return lamp1 == lamp2" ++
             "}" ++ 
             "val b1 = test(lamp1, lamp2);" ++ 
             "val b2 = test(lamp1, lamp2);" ++ 
             "return b2")
      
      tcErrorTest ("val lamp1 = Green;" ++ 
                   "val lamp2 = lamp1;" ++
                   "val lamp3 = lamp1;" ++ 
                   "return lamp3"
            )

      tcErrorTest  ("val mut lamp2 = Red;" ++
                    "fun test(l:Light) -> Light = {return Green}" ++
                    "val u2 = test(lamp2);" ++ 
                    "return lamp2"
            ) 

      tcTest  ("val mut lamp1 = Green;" ++ 
               "val mut lamp2 = Yellow;" ++
               "ass lamp2 = lamp1;" ++
               "ass lamp1 = Red;" ++
               "fun test(l:Light) -> Light = {return Green}" ++
               "val u1 = test(lamp1);" ++ 
               "val u2 = test(lamp2);" ++ 
               "return u2"
            ) TLight

      tcTest  ("val mut lamp1 = Green;" ++ 
               "val mut lamp2 = lamp1;" ++
               "ass lamp1 = Red;" ++
               "fun test(l:Light) -> Light = {return Green}" ++
               "val u1 = test(lamp1);" ++ 
               "val u2 = test(lamp2);" ++ 
               "return u2"
            ) TLight

      tcTest  ("val lamp1 = Green;" ++ 
               "val lamp2 = lamp1;" ++
               "val lamp1 = Red;" ++
               "fun test(l:Light) -> Light = {return Green}" ++
               "val u1 = test(lamp1);" ++ 
               "val u2 = test(lamp2);" ++ 
               "return u2"
            ) TLight

      tcTest  ("val lamp1 = Green;" ++ 
               "val mut lamp2 = lamp1;" ++
               "val lamp1 = Yellow;" ++ 
               "ass lamp2 = lamp1;" ++
               "return lamp2"
            ) TLight

      tcTest  ("val lamp1 = Red;" ++ 
               "val mut lamp2 = lamp1;" ++
               "ass lamp2 = Green;" ++ 
               "val lamp3 = lamp2;" ++ 
               "return lamp3"
            ) TLight

      tcTest  ("fun immutable(l:Light) -> Light = {return Red}" ++
               "fun mutable(mut l:Light) -> Light = {return Red}" ++ 
               "val imm_light = Red;" ++ 
               "val z = immutable(imm_light);" ++
               "val imm_light = Red;" ++ 
               "val zz = mutable(imm_light);" ++ 
               "val mut mut_light = Yellow;" ++
               "val zzz = immutable(mut_light);" ++ 
               "val mut mut_light = Yellow;" ++ 
               "val zzzz = mutable(mut_light);" ++ 
               "return Red"
            ) TLight

