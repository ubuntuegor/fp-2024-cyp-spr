module Main where 

import Text.Printf (printf)
import Control.Monad (unless)

data Expr
  = Number Double
  | Sqrt Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr

instance Show Expr where
  show (Number x) = show x
  show (Sqrt e) = "sqrt(" ++ show e ++ ")"
  show (Add lhs rhs) = "(" ++ show lhs ++ " + " ++ show rhs ++ ")"
  show (Sub lhs rhs) = "(" ++ show lhs ++ " - " ++ show rhs ++ ")"
  show (Mul lhs rhs) = "(" ++ show lhs ++ " * " ++ show rhs ++ ")"
  show (Div lhs rhs) = "(" ++ show lhs ++ " / " ++ show rhs ++ ")"
  show (Pow lhs rhs) = "(" ++ show lhs ++ " ^ " ++ show rhs ++ ")"

instance Eq Expr where
  Number x1 == Number x2 = x1 == x2
  Sqrt x1 == Sqrt x2 = x1 == x2
  Add lhs1 rhs1 == Add lhs2 rhs2 = (lhs1 == lhs2 && rhs1 == rhs2) || (lhs1 == rhs2 && rhs1 == lhs2)
  Sub lhs1 rhs1 == Sub lhs2 rhs2 = lhs1 == lhs2 && rhs1 == rhs2
  Mul lhs1 rhs1 == Mul lhs2 rhs2 = (lhs1 == lhs2 && rhs1 == rhs2) || (lhs1 == rhs2 && rhs1 == lhs2)
  Div lhs1 rhs1 == Div lhs2 rhs2 = lhs1 == lhs2 && rhs1 == rhs2
  Pow lhs1 rhs1 == Pow lhs2 rhs2 = lhs1 == lhs2 && rhs1 == rhs2
  (==) _ _ = False

data Error = ZeroDivision | RootOfNegative

instance Show Error where
  show ZeroDivision = "Cannot divide by zero"
  show RootOfNegative = "Cannot take the square root of a negative number"

instance Eq Error where
  ZeroDivision == ZeroDivision = True
  RootOfNegative == RootOfNegative = True
  (==) _ _ = False

eval :: Expr -> Either Error Double
eval (Number x) = Right x
eval (Sqrt e) = case eval e of
  Right x | x >= 0 -> Right (sqrt x)
  Right _ -> Left RootOfNegative
  err -> err
eval e = case e of
  Add lhs rhs -> doOp lhs rhs (\lx rx -> Right (lx + rx))
  Sub lhs rhs -> doOp lhs rhs (\lx rx -> Right (lx - rx))
  Mul lhs rhs -> doOp lhs rhs (\lx rx -> Right (lx * rx))
  Div lhs rhs -> doOp lhs rhs (\lx rx -> if rx /= 0 then Right (lx / rx) else Left ZeroDivision)
  Pow lhs rhs -> doOp lhs rhs (\lx rx -> Right (lx ** rx))
  where
    doOp lhs rhs op = case eval lhs of
      Right lx -> case eval rhs of
        Right rx -> op lx rx
        err -> err
      err -> err

cases :: [(Expr, Either Error Double)]
cases =
  [ (Number 2, Right 2),
    (Add (Number 2) (Number 4), Right 6),
    (Mul (Add (Number 2) (Number 2)) (Number 2), Right 8),
    (Div (Number 10) (Number 0), Left ZeroDivision),
    (Sqrt (Sub (Number 3) (Number 10)), Left RootOfNegative)
  ]

test :: Expr -> Either Error Double -> IO () 
test expr expected = 
    let actual = eval expr in 
    unless (expected == actual) $ describeFailure actual
  where 
    describeFailure actual = 
      printf "eval (%s) should be %s but it was %s" (show expr) (show expected) (show actual) 
  

main :: IO () 
main = do 
  mapM_ (uncurry test) cases 
  