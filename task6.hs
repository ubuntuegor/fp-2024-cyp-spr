rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs =
  if n >= 0
    then rotatePositive n xs []
    else
      let l = length xs
       in rotatePositive (n `mod` l) xs []
  where
    rotatePositive 0 xs acc = xs ++ reverse acc
    rotatePositive n [] acc = rotatePositive n (reverse acc) []
    rotatePositive n (x : xs) acc = rotatePositive (n - 1) xs (x : acc)

rotateTestCases :: [((Int, String), String)]
rotateTestCases =
  [ ((2, "abcdef"), "cdefab"),
    ((-2, "abcdef"), "efabcd"),
    ((0, "abcdef"), "abcdef"),
    ((6, "abcdef"), "abcdef"),
    ((8, "abcdef"), "cdefab"),
    ((-8, "abcdef"), "efabcd")
  ]

testRotate = all (\((n, xs), expected) -> rotate n xs == expected) rotateTestCases
