module Expr (Expr (..), EvalError (..), runEval) where

import StateDemo (State, execState, get)

data Expr a
  = Const a
  | Var String
  | Add (Expr a) (Expr a)
  | Sub (Expr a) (Expr a)
  | Mul (Expr a) (Expr a)
  | Div (Expr a) (Expr a)
  | Pow (Expr a) (Expr a)
  | Sqrt (Expr a)
  deriving (Eq, Show)

type ExprState = [(String, Double)]

data EvalError = ZeroDivision | RootOfNegative | UnknownVariable String deriving (Eq, Show)

eval :: (Integral a) => Expr a -> State ExprState (Either EvalError Double)
eval (Const x) = return $ return $ fromIntegral x
eval (Var var) = do
  env <- get
  let val = lookup var env
  case val of
    Nothing -> return $ Left $ UnknownVariable var
    Just x -> return $ return x
eval (Sqrt e) = do
  x <- eval e
  return $ do
    num <- x
    if num < 0
      then Left RootOfNegative
      else return $ sqrt num
eval binop =
  case binop of
    (Add e1 e2) -> evalBinop (\num1 num2 -> return $ num1 + num2) e1 e2
    (Sub e1 e2) -> evalBinop (\num1 num2 -> return $ num1 - num2) e1 e2
    (Mul e1 e2) -> evalBinop (\num1 num2 -> return $ num1 * num2) e1 e2
    (Div e1 e2) ->
      evalBinop
        ( \num1 num2 ->
            if num2 == 0
              then Left ZeroDivision
              else return $ num1 / num2
        )
        e1
        e2
    (Pow e1 e2) -> evalBinop (\num1 num2 -> return $ num1 ** num2) e1 e2
  where
    evalBinop f e1 e2 = do
      x1 <- eval e1
      x2 <- eval e2
      return $ do
        num1 <- x1
        num2 <- x2
        f num1 num2

runEval :: (Integral a) => ExprState -> Expr a -> Either EvalError Double
runEval s expr = execState (eval expr) s
