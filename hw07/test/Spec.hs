import Control.Monad (unless)
import Expr
import Parser (runExprParser)
import System.Exit (die)

testFunc :: (Show a, Show b, Eq b) => String -> (a -> b) -> a -> b -> IO Bool
testFunc testName f p expected = do
  let actual = f p
  if actual /= expected
    then putStrLn (testName ++ " with param " ++ show p ++ " should return " ++ show expected ++ " but got " ++ show actual) >> return False
    else return True

parserTestCases :: [(String, Either String (Expr Integer))]
parserTestCases =
  [ ("123", Right $ Const 123),
    ("xyz", Right $ Var "xyz"),
    ("sqrt 123", Right $ Sqrt $ Const 123),
    ("sqrt xyz1", Right $ Sqrt $ Var "xyz1"),
    ("+ 123 45", Right $ Add (Const 123) (Const 45)),
    ("- 123 45", Right $ Sub (Const 123) (Const 45)),
    ("* 123 45", Right $ Mul (Const 123) (Const 45)),
    ("/ 123 45", Right $ Div (Const 123) (Const 45)),
    ("^ 123 45", Right $ Pow (Const 123) (Const 45)),
    ("+ 123 * 45 6", Right $ Add (Const 123) (Mul (Const 45) (Const 6))),
    ("+  * 123 45   6", Right $ Add (Mul (Const 123) (Const 45)) (Const 6)),
    ("/ sqrt 123  xyz", Right $ Div (Sqrt (Const 123)) (Var "xyz")),
    ("sqrt", Left "Unexpected character"),
    ("- 123", Left "Unexpected character"),
    ("-sqrt123 45", Left "Unexpected character"),
    ("+ 45 1var", Left "Excessive input: var"),
    ("+ 123 45 6", Left "Excessive input:  6")
  ]

evalTestCases :: [(Expr Int, Either EvalError Double)]
evalTestCases =
  [ (Const 2, Right 2),
    (Var "answer", Right 42),
    (Var "undefined", Left (UnknownVariable "undefined")),
    (Add (Var "two") (Const 4), Right 6),
    (Sub (Var "two") (Const 4), Right (-2)),
    (Mul (Var "two") (Const 4), Right 8),
    (Div (Var "two") (Const 4), Right 0.5),
    (Pow (Var "two") (Const 4), Right 16),
    (Mul (Add (Var "two") (Const 2)) (Const 2), Right 8),
    (Div (Const 10) (Const 0), Left ZeroDivision),
    (Sqrt (Sub (Const 3) (Const 10)), Left RootOfNegative),
    (Add (Sub (Const 3) (Const 90)) (Div (Const 30) (Sub (Const 10) (Const 10))), Left ZeroDivision)
  ]

evalTestEnv :: [(String, Double)]
evalTestEnv = [("answer", 42), ("wrongAnswer", 39), ("two", 2)]

main :: IO ()
main = do
  parserTestsSuccessful <- mapM (uncurry $ testFunc "runExprParser" runExprParser) parserTestCases
  evalTestsSuccessful <- mapM (uncurry $ testFunc "runEval" (runEval evalTestEnv)) evalTestCases
  unless (and parserTestsSuccessful && and evalTestsSuccessful) $ die "Tests failed!"
