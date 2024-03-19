{-# LANGUAGE InstanceSigs #-}

module Logger where 

data Logger l a = Logger [l] a deriving (Show, Eq)

-- Implement the instance and prove the laws
instance Functor (Logger l) where 
  fmap :: (a -> b) -> Logger l a -> Logger l b
  fmap f (Logger log x) = Logger log $ f x

{-
fmap id = id
fmap id (Logger log x) = Logger log $ id x = Logger log x = id (Logger log x)
 -}

-- Implement the instance and prove the laws
instance Applicative (Logger l) where 
  pure :: a -> Logger l a
  pure = Logger []
  (<*>) :: Logger l (a -> b) -> Logger l a -> Logger l b
  (<*>) (Logger log1 f) (Logger log2 x) = Logger (log1 ++ log2) (f x)

{-
pure id <*> v = v
pure id <*> v =
[v = Logger log2 x] = Logger [] id <*> Logger log2 x = Logger ([] ++ log2) (id x) = Logger log2 x = v

pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
pure (.) <*> u <*> v <*> w = Logger [] (.) <*> u <*> v <*> w =
[u = Logger log1 f1, v = Logger log2 f2, w = Logger log3 x] = Logger ([] ++ log1) (f1.) <*> v <*> w = Logger (log1 ++ log2) (f1 . f2) <*> w = Logger (log1 ++ log2 ++ log3) ((f1 . f2) x) = u <*> Logger (log2 ++ log3) (f2 x) = u <*> (v <*> w)

pure f <*> pure x = pure (f x)
pure f <*> pure x = Logger [] f <*> Logger [] x = Logger [] (f x) = pure (f x)

u <*> pure y = pure ($ y) <*> u
u <*> pure y =
[u = Logger log f] = Logger log f <*> Logger [] y = Logger log (f y) = Logger [] ($y) <*> Logger log f = pure ($ y) <*> u
 -}

-- Implement the instance and prove the laws
instance Monad (Logger l) where 
  (>>=) :: Logger l a -> (a -> Logger l b) -> Logger l b
  (>>=) (Logger log1 x) f = case f x of
    Logger log2 y -> Logger (log1 ++ log2) y

{-
return a >>= k = k a
return a >>= k = Logger [] a >>= k =
[k a = Logger log y] = Logger ([] ++ log) y = Logger log y = k a

m >>= return = m
m >>= return =
[m = Logger log x] = Logger (log ++ []) x = m

m >>= (\x -> k x >>= h) = (m >>= k) >>= h
m >>= (\x -> k x >>= h) =
[m = Logger log1 x, k x = Logger log2 y, h y = Logger log3 z] = Logger (log1 ++ log2 ++ log3) z = (Logger (log1 ++ log2) y) >>= Logger log3 z = (m >>= k) >>= h
 -}

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

          