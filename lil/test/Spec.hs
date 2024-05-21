import Control.Monad.Except
import Control.Monad.State
import Interpreter (Env (Env), evalProgram)
import Parser (parseProgram)
import System.Exit (die)
import System.IO (stdout)
import Text.Megaparsec (errorBundlePretty, runParser)

cases :: [(String, Either String Int)]
cases =
  [ ("(2 + 2) * 90 / 5", Right 72),
    ("number = 42; newNumber = 21; number + newNumber", Right 63),
    ("20 / 0", Left "Divide by zero"),
    ("20 % 0", Left "Divide by zero"),
    ("write unk", Left "Undefined variable: unk"),
    ("number = 10; if (number + 10 == 20) { 33 } else {44}", Right 33),
    ("i = 1; while (i < 10) { i = i + 1; write i }; i", Right 10),
    ("function hello() { 99 } hello() + 1", Right 100),
    ("function test() { write number } number = 10; test()", Left "Undefined variable: number"),
    ("function test(a, b) { a + b } test(9, 10)", Right 19)
  ]

testCase :: (String, Either String Int) -> IO Bool
testCase (input, expected) =
  case runParser parseProgram "test input" input of
    Left err -> False <$ putStrLn (errorBundlePretty err)
    Right program -> do
      result <- evalStateT (runExceptT $ evalProgram program) (Env [] [] stdout)
      if result /= expected
        then putStrLn (input ++ " should evaluate to " ++ show expected ++ " but instead got " ++ show result) >> return False
        else return True

main :: IO ()
main = do
  pass <- mapM testCase cases
  if and pass
    then putStrLn "Pass"
    else die "Some tests failed"
