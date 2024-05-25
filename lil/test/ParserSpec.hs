module ParserSpec (spec) where

import Parser
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

spec :: Spec
spec = describe "parseProgram" $ do
  it "parses numbers" $
    parse parseProgram "" "1" `shouldParse` Program [] (Seq [Expr' (Const 1)])
  it "parses variables" $
    parse parseProgram "" "something" `shouldParse` Program [] (Seq [Expr' (Var "something")])
  it "parses function calls" $
    parse parseProgram "" "callMe(a, b)" `shouldParse` Program [] (Seq [Expr' (Call "callMe" [Var "a", Var "b"])])
  it "parses binops" $
    parse parseProgram "" "13 + a * 7 / (90 - 22)" `shouldParse` Program [] (Seq [Expr' (Binop Add (Const 13) (Binop Div (Binop Mul (Var "a") (Const 7)) (Binop Sub (Const 90) (Const 22))))])
  it "parses assignments" $
    parse parseProgram "" "myVar = 90" `shouldParse` Program [] (Seq [Asgn "myVar" (Const 90)])
  it "parses write" $
    parse parseProgram "" "write myVar" `shouldParse` Program [] (Seq [Write (Var "myVar")])
  it "parses read" $
    parse parseProgram "" "read myVar" `shouldParse` Program [] (Seq [Read "myVar"])
  it "parses while" $
    parse parseProgram "" "while (i < 10) {}" `shouldParse` Program [] (Seq [While (Binop Lt (Var "i") (Const 10)) (Seq [])])
  it "parses while with body" $
    parse parseProgram "" "while (i < 10) { write i }" `shouldParse` Program [] (Seq [While (Binop Lt (Var "i") (Const 10)) (Seq [Write (Var "i")])])
  it "parses if" $
    parse parseProgram "" "if (i < 10) {}" `shouldParse` Program [] (Seq [If (Binop Lt (Var "i") (Const 10)) (Seq []) Nothing])
  it "parses if with body" $
    parse parseProgram "" "if (i < 10) { write i }" `shouldParse` Program [] (Seq [If (Binop Lt (Var "i") (Const 10)) (Seq [Write (Var "i")]) Nothing])
  it "parses if else" $
    parse parseProgram "" "if (i < 10) {} else {}" `shouldParse` Program [] (Seq [If (Binop Lt (Var "i") (Const 10)) (Seq []) (Just (Seq []))])
  it "parses if elseif" $
    parse parseProgram "" "if (i < 10) {} else if (i > 10) {}"
      `shouldParse` Program
        []
        ( Seq
            [ If
                (Binop Lt (Var "i") (Const 10))
                (Seq [])
                ( Just
                    ( If
                        (Binop Gt (Var "i") (Const 10))
                        (Seq [])
                        Nothing
                    )
                )
            ]
        )
  it "parses if elseif else" $
    parse parseProgram "" "if (i < 10) {} else if (i > 10) {} else {}"
      `shouldParse` Program
        []
        ( Seq
            [ If
                (Binop Lt (Var "i") (Const 10))
                (Seq [])
                ( Just
                    ( If
                        (Binop Gt (Var "i") (Const 10))
                        (Seq [])
                        (Just (Seq []))
                    )
                )
            ]
        )
  it "parses skip" $
    parse parseProgram "" "skip" `shouldParse` Program [] (Seq [Skip])
  it "parses sequence" $
    parse parseProgram "" "read a; a = a + 10; write a" `shouldParse` Program [] (Seq [Read "a", Asgn "a" (Binop Add (Var "a") (Const 10)), Write (Var "a")])
  it "parses functions" $
    parse parseProgram "" "function myFunc(a, b, c) { write a }" `shouldParse` Program [Func "myFunc" ["a", "b", "c"] (Seq [Write (Var "a")])] (Seq [])
  it "skips whitespace" $
    parse parseProgram "" "\n function   myFunc  (  a , b , c  ) \n {    write a   +\n   2    }  \n  " `shouldParse` Program [Func "myFunc" ["a", "b", "c"] (Seq [Write (Binop Add (Var "a") (Const 2))])] (Seq [])
