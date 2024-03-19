module IO where

-- Implement reading line from standard input.
-- Use getChar to read a single character.
myGetLine :: IO String
myGetLine = do
  char <- getChar
  if char == '\n'
    then return ""
    else getMore char
  where
    getMore pre = do
      rest <- myGetLine
      return $ pre : rest

-- Ask the user for their name.
-- Print "Hello, NAME" to the standard output, where NAME is the name of the user.
-- Use myGetLine.
helloUser :: IO ()
helloUser = do
    name <- myGetLine
    putStr "Hello, "
    putStr name
    putStrLn ""
    return ()

-- Use interact in helloUser.
helloUser' :: IO ()
helloUser' = do
    interact ("Hello, " ++)
    return ()
