module Main (main) where

import Control.Monad.Except
import Control.Monad.State
import Interpreter (Env (Env), evalProgram)
import Options.Applicative
import Parser (Program, parseProgram)
import System.Exit (die)
import System.IO (Handle, IOMode (WriteMode), stdout, withFile)
import Text.Megaparsec

data Options = Options
  { inputFile :: FilePath,
    outputFile :: Maybe FilePath
  }

options :: Parser Options
options =
  Options
    <$> strOption
      ( metavar "INPUT_FILE"
          <> help "Program file"
          <> short 'i'
          <> long "input"
      )
    <*> optional
      ( strOption
          ( long "output"
              <> short 'o'
              <> metavar "OUTPUT_FILE"
              <> help "Write output to file instead of stdout"
          )
      )

opts :: ParserInfo Options
opts = info (options <**> helper) (fullDesc <> progDesc "Execute a program")

main :: IO ()
main = execParser opts >>= run

run :: Options -> IO ()
run opt@(Options input _) = do
  code <- readFile input
  let result = runParser parseProgram (show input) code
  case result of
    Left err -> putStrLn $ errorBundlePretty err
    Right program -> runWithOutput opt program

runWithOutput :: Options -> Program -> IO ()
runWithOutput (Options _ output) program = do
  case output of
    Just path -> withFile path WriteMode (runProgram program)
    Nothing -> runProgram program stdout

runProgram :: Program -> Handle -> IO ()
runProgram program handle = do
  result <- evalStateT (runExceptT $ evalProgram program) (Env [] [] handle)
  case result of
    Right _ -> return ()
    Left err -> die err
