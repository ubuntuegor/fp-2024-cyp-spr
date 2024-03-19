{-# LANGUAGE InstanceSigs #-}

module Either where

data MyEither a b
  = MyLeft a
  | MyRight b
  deriving (Show, Eq)

-- Implement the instance and prove the laws
instance Functor (MyEither a) where
  fmap :: (b -> c) -> MyEither a b -> MyEither a c
  fmap f (MyRight x) = MyRight $ f x
  fmap f (MyLeft x) = MyLeft x

{-
fmap id = id
fmap id (MyRight x) = MyRight $ id x = MyRight x = id (MyRight x)
fmap id (MyLeft x) = MyLeft x = id (MyLeft x)
 -}

-- Implement the instance and prove the laws
instance Applicative (MyEither a) where
  pure :: b -> MyEither a b
  pure = MyRight
  (<*>) :: MyEither a (b -> c) -> MyEither a b -> MyEither a c
  (<*>) (MyLeft x) _ = MyLeft x
  (<*>) _ (MyLeft x) = MyLeft x
  (<*>) (MyRight f) (MyRight x) = MyRight $ f x

{-
pure id <*> v = v
pure id <*> v = MyRight id <*> v =
[v = MyRight x] = MyRight $ id x = MyRight x = v
[v = MyLeft x] = MyLeft x = v

pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
pure (.) <*> u <*> v <*> w = MyRight (.) <*> u <*> v <*> w =
[u = MyLeft x] = MyLeft x = u <*> (v <*> w)
[u = MyRight f1, v = MyLeft x] = MyRight (f1.) <*> v <*> w = MyLeft x = u <*> (v <*> w)
[u = MyRight f1, v = MyRight f2, w = MyLeft x] = MyRight (f1 . f2) <*> w = MyLeft x = u <*> (v <*> w)
[u = MyRight f1, v = MyRight f2, w = MyRight x] = MyRight ((f1 . f2) x) = MyRight (f1 (f2 x)) = u <*> MyRight (f2 x) = u <*> (v <*> w)

pure f <*> pure x = pure (f x)
pure f <*> pure x = MyRight f <*> MyRight x = MyRight (f x) = pure (f x)

u <*> pure y = pure ($ y) <*> u
u <*> pure y =
[u = MyLeft x] = MyLeft x = pure ($ y) <*> u
[u = MyRight f] = MyRight f <*> MyRight y = MyRight (f y) = MyRight (($y) f) = MyRight ($y) <*> MyRight f = pure ($ y) <*> u
 -}

-- Implement the instance and prove the laws
instance Monad (MyEither a) where
  (>>=) :: MyEither a b -> (b -> MyEither a c) -> MyEither a c
  (>>=) (MyLeft x) _ = MyLeft x
  (>>=) (MyRight x) f = f x

{-
return a >>= k = k a
return a >>= k = MyRight a >>= k = k a

m >>= return = m
m >>= return =
[m = MyLeft x] = MyLeft x >>= MyRight = MyLeft x = m
[m = MyRight x] = MyRight x >>= MyRight = MyRight x = m

m >>= (\x -> k x >>= h) = (m >>= k) >>= h
m >>= (\x -> k x >>= h) =
[m = MyLeft x] = MyLeft x = (m >>= k) >>= h
[m = MyRight x] = k x >>= h = (m >>= k) >>= h
 -}
