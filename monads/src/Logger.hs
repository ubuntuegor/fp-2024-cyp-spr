{-# LANGUAGE InstanceSigs #-}

module Logger where 

data Logger l a = Logger [l] a deriving (Show, Eq)

-- Implement the instance and prove the laws
instance Functor (Logger l) where 
  fmap :: (a -> b) -> Logger l a -> Logger l b
  fmap f (Logger log x) = Logger log $ f x

-- Implement the instance and prove the laws
instance Applicative (Logger l) where 
  pure :: a -> Logger l a
  pure = Logger []
  (<*>) :: Logger l (a -> b) -> Logger l a -> Logger l b
  (<*>) (Logger log1 f) (Logger log2 x) = Logger (log1 ++ log2) (f x)

-- Implement the instance and prove the laws
instance Monad (Logger l) where 
  (>>=) :: Logger l a -> (a -> Logger l b) -> Logger l b
  (>>=) (Logger log1 x) f = case f x of
    Logger log2 y -> Logger (log1 ++ log2) y

-- Writes a single log message. 
-- Can be easily bound together with other logging computations.
writeLog :: l -> Logger l ()
writeLog l = Logger [l] () 

-- Logs every intermediate result 
-- ghci> factLog 5
-- Logger [(0,1),(1,1),(2,2),(3,6),(4,24),(5,120)] 120
factLog :: Int -> Logger (Int, Int) Int 
factLog n 
  | n <= 0 = do 
      let res = 1 
      writeLog (n, res)
      return res 
  | otherwise = do 
      prev <- factLog (n - 1)
      let res = n * prev 
      writeLog (n, res)
      return res 

          