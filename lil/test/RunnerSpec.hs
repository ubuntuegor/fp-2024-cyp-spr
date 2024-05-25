module RunnerSpec (spec) where

import Runner (interpret)
import System.IO (stdout)
import Test.Hspec

spec :: Spec
spec = describe "interpret" $ do
  it "should do expressions" $
    intepret' "(2 + 2) * 90 / 5" `shouldReturn` Right 72
  it "should work with variables" $
    intepret' "number = 42; newNumber = 21; number + newNumber" `shouldReturn` Right 63
  it "fails on divide by zero" $
    intepret' "20 / 0" `shouldReturn` Left "Divide by zero"
  it "fails on mod by zero" $
    intepret' "20 % 0" `shouldReturn` Left "Divide by zero"
  it "fails on unknown variable" $
    intepret' "write unk" `shouldReturn` Left "Undefined variable: unk"
  it "supports if" $
    intepret' "number = 10; if (number + 10 == 20) { 33 } else {44}" `shouldReturn` Right 33
  it "supports while" $
    intepret' "i = 1; while (i < 10) { i = i + 1 }; i" `shouldReturn` Right 10
  it "supports function return value" $
    intepret' "function hello() { 99 } hello() + 1" `shouldReturn` Right 100
  it "fails when function tries to access foreign variable" $
    intepret' "function test() { write number } number = 10; test()" `shouldReturn` Left "Undefined variable: number"
  it "supports function with arguments" $
    intepret' "function test(a, b) { a + b } test(9, 10)" `shouldReturn` Right 19
  it "fails when function is called with wrong number of arguments" $
    intepret' "function test(a, b) { a + b } test()" `shouldReturn` Left "Insufficient number of arguments passed to function test"
  where
    intepret' code = interpret code "" stdout
