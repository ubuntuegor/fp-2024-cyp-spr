module Expr where 
import StateDemo ( State, execState, get, modify )
import Data.Maybe ( fromJust )

data Expr = V String | C Int | Plus Expr Expr 
          | Let String Expr Expr 
        
type ExprState = [(String, Int)]

eval :: Expr -> State ExprState Int
eval (V v) = do 
  env <- get 
  return $ fromJust $ lookup v env
eval (C x) = return x 
eval (Plus x y) = do 
  x <- eval x 
  y <- eval y 
  return $ x + y 
eval (Let x v b) = do 
  v <- eval v 
  modify ((x, v) :)
  eval b 

-- runEval (Let "x" (C 13) (Plus (V "x") (C 42)))
runEval :: Expr -> Int
runEval expr = 
  execState (eval expr) [] 