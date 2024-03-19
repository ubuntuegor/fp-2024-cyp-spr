{-# LANGUAGE InstanceSigs #-}

module List where

data List a
  = Nil
  | Cons a (List a)
  deriving (Show, Eq)

-- Implement the instance and prove the laws
instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- Implement the instance and prove the laws
instance Applicative List where
  pure :: a -> List a
  pure x = Cons x Nil
  (<*>) :: List (a -> b) -> List a -> List b
  (<*>) (Cons f fs) xs =
    applyf f xs (fs <*> xs)
    where
      applyf f (Cons x xs) end = Cons (f x) (applyf f xs end)
      applyf f Nil end = end

-- Implement the instance and prove the laws
instance Monad List where
  (>>=) :: List a -> (a -> List b) -> List b
  (>>=) (Cons x _) f = f x
  (>>=) Nil _ = Nil
