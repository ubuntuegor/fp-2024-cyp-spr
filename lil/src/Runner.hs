module Runner (interpret) where

import Control.Monad.Except (runExceptT)
import Control.Monad.State (evalStateT)
import Interpreter (Env (Env), evalProgram)
import Parser (parseProgram)
import System.IO (Handle)
import Text.Megaparsec (errorBundlePretty, runParser)

interpret :: String -> String -> Handle -> IO (Either String Int)
interpret program fileName handle =
  case runParser parseProgram fileName program of
    Left err -> return $ Left $ errorBundlePretty err
    Right p -> runProgram p
  where
    runProgram p = evalStateT (runExceptT $ evalProgram p) (Env [] [] handle)
