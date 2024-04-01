module Main (main) where

import Expr (EvalError (..), runEval)
import Parser (runExprParser)
import System.Exit (die)

explainEvalError :: EvalError -> String
explainEvalError ZeroDivision = "Cannot divide by zero"
explainEvalError RootOfNegative = "Cannot take square root of a negative number"
explainEvalError (UnknownVariable var) = "Met unknown variable: " ++ var

main :: IO ()
main = do
  putStrLn "Enter an expression in prefix notation:"
  exprString <- getLine
  let expr = runExprParser exprString
  case expr of
    Left err -> die err
    Right x -> evalExpr x
  where
    evalExpr expr = do
      env <- collectEnv
      putStrLn "Got environment:"
      print env
      case runEval env expr of
        Left err -> die $ explainEvalError err
        Right x -> putStrLn $ "Result: " ++ show x
    collectEnv = do
      putStrLn "Enter an initial environment, variable name on one line, then its value on the second line, repeat as many times as you need. Enter a blank line when done:"
      collectEnv' []
    collectEnv' env = do
      entry <- readVar
      case entry of
        Nothing -> return env
        Just x -> collectEnv' (x : env)
    readVar = do
      name <- getLine
      if null name
        then return Nothing
        else do
          val <- getLine
          return $ Just (name, read val)
