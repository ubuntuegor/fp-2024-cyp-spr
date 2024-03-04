module Main where

import Control.Monad (unless)
import Text.Printf (printf)

newtype MyEither a b = MyEither (Either a b) deriving (Show, Eq)

instance Functor (MyEither a) where
  fmap :: (a2 -> b) -> MyEither a1 a2 -> MyEither a1 b
  fmap f (MyEither (Right x)) = MyEither $ Right $ f x
  fmap _ (MyEither (Left x)) = MyEither $ Left x

newtype MyArrow a b = MyArrow {getArrow :: a -> b}

instance Functor (MyArrow a) where
  fmap :: (b -> c) -> MyArrow a b -> MyArrow a c
  fmap f2 (MyArrow f1) = MyArrow $ f2 . f1

data Expr a
  = Number a
  | Var String
  | Sqrt (Expr a)
  | Add (Expr a) (Expr a)
  | Sub (Expr a) (Expr a)
  | Mul (Expr a) (Expr a)
  | Div (Expr a) (Expr a)
  | Pow (Expr a) (Expr a)

instance (Show a) => Show (Expr a) where
  show (Number x) = show x
  show (Var x) = x
  show (Sqrt e) = "sqrt(" ++ show e ++ ")"
  show (Add lhs rhs) = "(" ++ show lhs ++ " + " ++ show rhs ++ ")"
  show (Sub lhs rhs) = "(" ++ show lhs ++ " - " ++ show rhs ++ ")"
  show (Mul lhs rhs) = "(" ++ show lhs ++ " * " ++ show rhs ++ ")"
  show (Div lhs rhs) = "(" ++ show lhs ++ " / " ++ show rhs ++ ")"
  show (Pow lhs rhs) = "(" ++ show lhs ++ " ^ " ++ show rhs ++ ")"

instance (Eq a) => Eq (Expr a) where
  Number x1 == Number x2 = x1 == x2
  Var x1 == Var x2 = x1 == x2
  Sqrt x1 == Sqrt x2 = x1 == x2
  Add lhs1 rhs1 == Add lhs2 rhs2 = (lhs1 == lhs2 && rhs1 == rhs2) || (lhs1 == rhs2 && rhs1 == lhs2)
  Sub lhs1 rhs1 == Sub lhs2 rhs2 = lhs1 == lhs2 && rhs1 == rhs2
  Mul lhs1 rhs1 == Mul lhs2 rhs2 = (lhs1 == lhs2 && rhs1 == rhs2) || (lhs1 == rhs2 && rhs1 == lhs2)
  Div lhs1 rhs1 == Div lhs2 rhs2 = lhs1 == lhs2 && rhs1 == rhs2
  Pow lhs1 rhs1 == Pow lhs2 rhs2 = lhs1 == lhs2 && rhs1 == rhs2
  (==) _ _ = False

data Error = ZeroDivision | RootOfNegative | UnknownVariable String deriving (Eq)

instance Show Error where
  show ZeroDivision = "Cannot divide by zero"
  show RootOfNegative = "Cannot take the square root of a negative number"
  show (UnknownVariable var) = "Unknown variable: " ++ var

eval :: (Floating a, Ord a) => Expr a -> [(String, a)] -> Either Error a
eval (Number x) _ = Right x
eval (Var x) env = case find ((== x) . fst) env of
  Just x -> Right $ snd x
  Nothing -> Left $ UnknownVariable x
  where
    find pred (x : xs) = if pred x then Just x else find pred xs
    find _ _ = Nothing
eval (Sqrt e) env = case eval e env of
  Right x | x >= 0 -> Right (sqrt x)
  Right _ -> Left RootOfNegative
  err -> err
eval e env = case e of
  Add lhs rhs -> doOp lhs rhs (\lx rx -> Right (lx + rx))
  Sub lhs rhs -> doOp lhs rhs (\lx rx -> Right (lx - rx))
  Mul lhs rhs -> doOp lhs rhs (\lx rx -> Right (lx * rx))
  Div lhs rhs -> doOp lhs rhs (\lx rx -> if rx /= 0 then Right (lx / rx) else Left ZeroDivision)
  Pow lhs rhs -> doOp lhs rhs (\lx rx -> Right (lx ** rx))
  where
    doOp lhs rhs op = case eval lhs env of
      Right lx -> case eval rhs env of
        Right rx -> op lx rx
        err -> err
      err -> err

cases :: (Floating a) => [(Expr a, Either Error a)]
cases =
  [ (Number 2, Right 2),
    (Var "answer", Right 42),
    (Var "undefined", Left (UnknownVariable "undefined")),
    (Add (Var "two") (Number 4), Right 6),
    (Sub (Var "two") (Number 4), Right (-2)),
    (Mul (Var "two") (Number 4), Right 8),
    (Div (Var "two") (Number 4), Right 0.5),
    (Pow (Var "two") (Number 4), Right 16),
    (Mul (Add (Var "two") (Number 2)) (Number 2), Right 8),
    (Div (Number 10) (Number 0), Left ZeroDivision),
    (Sqrt (Sub (Number 3) (Number 10)), Left RootOfNegative),
    (Add (Sub (Number 3) (Number 90)) (Div (Number 30) (Sub (Number 10) (Number 10))), Left ZeroDivision)
  ]

test :: (Floating a, Ord a, Show a) => Expr a -> Either Error a -> IO ()
test expr expected =
  let actual = eval expr [("answer", 42), ("wrongAnswer", 39), ("two", 2)]
   in unless (expected == actual) $ describeFailure actual
  where
    describeFailure actual =
      printf "eval (%s) should be %s but it was %s\n" (show expr) (show expected) (show actual)

simplify :: (Floating a, Eq a) => Expr a -> Expr a
simplify (Sqrt x) =
  case simplify x of
    Number 0 -> Number 0
    Number 1 -> Number 1
    x -> Sqrt x
simplify (Add lhs rhs) =
  case (simplify lhs, simplify rhs) of
        (Number 0, rhs) -> rhs
        (lhs, Number 0) -> lhs
        (lhs, rhs) -> Add lhs rhs
simplify (Sub lhs rhs) =
  case (simplify lhs, simplify rhs) of
        (lhs, Number 0) -> lhs
        (lhs, rhs) -> Sub lhs rhs
simplify (Mul lhs rhs) =
  case (simplify lhs, simplify rhs) of
        (Number 0, _) -> Number 0
        (_, Number 0) -> Number 0
        (Number 1, rhs) -> rhs
        (lhs, Number 1) -> lhs
        (lhs, rhs) -> Mul lhs rhs
simplify (Div lhs rhs) =
  case (simplify lhs, simplify rhs) of
        (lhs, Number 1) -> lhs
        (lhs, rhs) -> Div lhs rhs
simplify (Pow lhs rhs) =
  case (simplify lhs, simplify rhs) of
        (_, Number 0) -> Number 1
        (lhs, Number 1) -> lhs
        (Number 1, _) -> Number 1
        (lhs, rhs) -> Pow lhs rhs
simplify x = x

simplifyRegressCases = map (\(c, a) -> (simplify c, a)) cases

simplifyCases =
  [ (Add (Number 0) (Var "a"), Var "a"),
    (Add (Var "a") (Number 0), Var "a"),
    (Sqrt (Number 0), Number 0),
    (Sqrt (Number 1), Number 1),
    (Sub (Number 39) (Number 0), Number 39),
    (Mul (Number 39) (Number 0), Number 0),
    (Mul (Number 0) (Number 39), Number 0),
    (Mul (Number 39) (Number 1), Number 39),
    (Mul (Number 1) (Number 39), Number 39),
    (Div (Number 39) (Number 1), Number 39),
    (Pow (Number 39) (Number 1), Number 39),
    (Pow (Number 39) (Number 0), Number 1),
    (Pow (Number 1) (Number 39), Number 1),
    (Add (Number 39) (Mul (Number 42) (Number 0)), Number 39),
    (Add (Number 1) (Number 39), Add (Number 1) (Number 39)),
    (Mul (Add (Var "two") (Number 2)) (Number 2), Mul (Add (Var "two") (Number 2)) (Number 2)),
    (Add (Sub (Number 3) (Number 90)) (Div (Number 30) (Add (Number 10) (Number (-10)))), Add (Sub (Number 3) (Number 90)) (Div (Number 30) (Add (Number 10) (Number (-10)))))
  ]

testSimplify :: (Floating a, Eq a, Show a) => Expr a -> Expr a -> IO ()
testSimplify expr expected =
  let actual = simplify expr
   in unless (expected == actual) $ describeFailure actual
  where
    describeFailure actual =
      printf "simplify (%s) should be %s but it was %s\n" (show expr) (show expected) (show actual)

instance (Num a) => Num (Expr a) where
  (+) :: Expr a -> Expr a -> Expr a
  (+) = Add
  (-) :: Expr a -> Expr a -> Expr a
  (-) = Sub
  (*) :: Expr a -> Expr a -> Expr a
  (*) = Mul
  abs :: Expr a -> Expr a
  abs = undefined
  signum :: Expr a -> Expr a
  signum = undefined
  fromInteger :: Integer -> Expr a
  fromInteger = Number . fromInteger

main :: IO ()
main = do
  mapM_ (uncurry test) cases
  mapM_ (uncurry test) simplifyRegressCases
  mapM_ (uncurry testSimplify) simplifyCases

bruh x = case x of
  Add (Number 1) (Number 2) -> True
  _ -> False
