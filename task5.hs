import Data.List (genericIndex)
import Text.Printf (printf)

data BInteger = BInteger Int Integer
  deriving (Eq)

numchars = "0123456789abcdefghijklmnopqrstuvwxyz"

instance Show BInteger where
  show (BInteger _ 0) = "0"
  show (BInteger base x) | x < 0 = '-' : show (BInteger base (-x))
  show (BInteger base x) =
    if base < 2 || base > 36
      then error "Base should be between 2 and 36"
      else notZero (BInteger base $ x `div` toInteger base) ++ [numchars `genericIndex` (x `mod` toInteger base)]
    where
      notZero x =
        let str = show x
         in if str == "0" then "" else str

showTestCases =
  [ (BInteger 2 5, "101"),
    (BInteger 16 0, "0"),
    (BInteger 36 35, "z"),
    (BInteger 10 999, "999"),
    (BInteger 2 (-16), "-10000")
  ]

instance Num BInteger where
  (+) (BInteger lbase lhs) (BInteger rbase rhs) =
    if lbase /= rbase
      then error $ "Cannot add different bases " ++ show lbase ++ ", " ++ show rbase
      else BInteger lbase (lhs + rhs)
  (*) (BInteger lbase lhs) (BInteger rbase rhs) =
    if lbase /= rbase
      then error $ "Cannot multiply different bases " ++ show lbase ++ ", " ++ show rbase
      else BInteger lbase (lhs * rhs)
  abs (BInteger base x) = BInteger base (abs x)
  signum (BInteger base x) = BInteger base (signum x)
  fromInteger = BInteger 10
  negate (BInteger base x) = BInteger base (negate x)

ten = BInteger 16 10

two = BInteger 16 2

numTestCases =
  [ (ten + two, "c"),
    (ten - two, "8"),
    (ten * two, "14"),
    (-ten, "-a"),
    (abs (-ten), "a"),
    (signum (-ten), "-1")
  ]

instance Ord BInteger where
  compare (BInteger lbase lhs) (BInteger rbase rhs) =
    if lbase /= rbase
      then error $ "Cannot compare different bases " ++ show lbase ++ ", " ++ show rbase
      else compare lhs rhs

ordTestCases =
  [ (ten > two, True),
    (ten >= two, True),
    (two > two, False),
    (two < ten, True),
    (two == two, True),
    (two == ten, False)
  ]

testBInteger =
  all (\(i, expected) -> show i == expected) showTestCases
    && all (\(i, expected) -> show i == expected) numTestCases
    && all (uncurry (==)) ordTestCases
