module Main (main) where

import Options.Applicative
    ( optional,
      (<**>),
      fullDesc,
      help,
      info,
      long,
      metavar,
      progDesc,
      short,
      strOption,
      execParser,
      helper,
      Parser,
      ParserInfo )
import Runner (interpret)
import System.Exit (die)
import System.IO (IOMode (WriteMode), stdout, withFile)

data Options = Options
  { inputFile :: FilePath,
    outputFile :: Maybe FilePath
  }

options :: Parser Options
options =
  Options
    <$> strOption
      ( short 'i'
          <> long "input"
          <> metavar "INPUT_FILE"
          <> help "Program file"
      )
    <*> optional
      ( strOption
          ( short 'o'
              <> long "output"
              <> metavar "OUTPUT_FILE"
              <> help "Write output to file instead of stdout"
          )
      )

optionsParser :: ParserInfo Options
optionsParser = info (options <**> helper) (fullDesc <> progDesc "Execute a program in L language")

main :: IO ()
main = execParser optionsParser >>= run

run :: Options -> IO ()
run opts = do
  code <- readFile $ inputFile opts
  result <- withHandle $ interpret code (show $ inputFile opts)
  case result of
    Right _ -> return ()
    Left err -> die err
  where
    withHandle f =
      case outputFile opts of
        Just path -> withFile path WriteMode f
        Nothing -> f stdout
