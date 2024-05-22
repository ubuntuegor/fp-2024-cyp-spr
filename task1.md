```haskell
func1 :: [a -> b] -> a -> [b]
func1 xs base = map ($ base) xs

func2 :: (a -> b) -> Tree a -> [b]
func2 _ Leaf = []
func2 f (Node a b c) = (func2 f a) ++ f b : (func2 f c)

func3 :: Ord a => [a] -> [a]
func3 = id

func4 :: (a -> b -> c) -> (a -> b) -> a -> c
func4 f2 f1 x = f2 x (f1 x)

func5 :: ((a -> b) -> c -> d) -> (a -> c -> b) -> c -> d
func5 f2 f1 x = (f2 $ (flip f1) x ) x
```
