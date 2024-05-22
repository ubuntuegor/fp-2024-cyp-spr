1.  ```
    ((), Int)  
    to i = ((), x)  
    from ((), i) = i
    ```

2.  ```
    Tree' a = Maybe (a, Tree' a, Tree' a)  
    to (Leaf) = Nothing  
    to (Node l c r) = Just (c, to l, to r)  
    from (Nothing) = Leaf  
    from (Just (c, l, r)) = Node (from l) c (from r)
    ```

3.  ```
    List' a = Maybe (a, List' a)  
    to (Nil) = Nothing  
    to (Cons x xs) = Just (x, to xs)  
    from (Nothing) = Nil  
    from (Just (x, xs)) = Cons x (from xs)
    ```

4.  ```
    |Either a (Either b (c, d))| = |a| + |b| + |c| * |d|  
    Either3 = One a | Two b | Three c d
    |Either3| = |a| + |b| + |c| * |d|  
    to (Left x) = One x  
    to (Right (Left x)) = Two x  
    to (Right (Right (x, y))) = Three x y  
    from (One x) = Left x  
    from (Two x) = Right (Left x)  
    from (Three x y) = Right (Right (x, y))
    ```

5.  ```
    |(a -> b, a -> c)| = |b| ^ |a| * |c| ^ |a|  
    |a -> (b, c)| = (|b| * |c|) ^ |a| = |b| ^ |a| * |c| ^ |a|
    ```
