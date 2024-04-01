{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
module Parser (runExprParser) where

import Control.Applicative (Alternative (..))
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Expr

newtype Parser a = Parser {runParser :: String -> Either String (String, a)}

keywords :: [String]
keywords = ["sqrt"]

instance Functor Parser where
  fmap f p = Parser $ \text -> fmap (fmap f) (runParser p text)

instance Applicative Parser where
  pure x = Parser $ \text -> Right (text, x)
  (<*>) p1 p2 = Parser $ \text -> do
    (text', f) <- runParser p1 text
    (text'', x) <- runParser p2 text'
    return (text'', f x)

instance Alternative Parser where
  empty = Parser $ \_ -> Left "Empty parser"
  (<|>) p1 p2 = Parser $ \text ->
    case runParser p1 text of
      Right x -> Right x
      Left _ -> runParser p2 text

instance Monad Parser where
  (>>=) p1 f = Parser $ \text -> do
    (text', x) <- runParser p1 text
    runParser (f x) text'

failParser :: String -> Parser a
failParser err = Parser $ \_ -> Left err

parseIf :: (Char -> Bool) -> Parser Char
parseIf predicate = Parser $ \text ->
  case text of
    (h : t) | predicate h -> Right (t, h)
    _ -> Left "Unexpected character"

parseChar :: Char -> Parser Char
parseChar c = parseIf (c ==)

parseToken :: String -> Parser String
parseToken token =
  let parsers = map parseChar token
   in sequence parsers

ws :: Parser String
ws = some (parseIf isSpace)

parseNumber :: Parser (Expr Integer)
parseNumber = Const . read <$> some (parseIf isDigit)

parseVar :: Parser (Expr Integer)
parseVar = do
  h <- parseIf isAlpha
  t <- many $ parseIf isAlphaNum
  let var = h : t
  if var `elem` keywords
    then failParser $ "Variable name cannot be a keyword: " ++ var
    else return $ Var var

parseSqrt :: Parser (Expr Integer)
parseSqrt = do
  _ <- parseToken "sqrt"
  _ <- ws
  Sqrt <$> parseExpr

parseBinop :: Char -> (Expr Integer -> Expr Integer -> Expr Integer) -> Parser (Expr Integer)
parseBinop c e = do
  _ <- parseChar c
  _ <- ws
  e1 <- parseExpr
  _ <- ws
  e e1 <$> parseExpr

parseAdd :: Parser (Expr Integer)
parseAdd = parseBinop '+' Add

parseSub :: Parser (Expr Integer)
parseSub = parseBinop '-' Sub

parseMul :: Parser (Expr Integer)
parseMul = parseBinop '*' Mul

parseDiv :: Parser (Expr Integer)
parseDiv = parseBinop '/' Div

parsePow :: Parser (Expr Integer)
parsePow = parseBinop '^' Pow

parseExpr :: Parser (Expr Integer)
parseExpr =
  parseNumber
    <|> parseVar
    <|> parseSqrt
    <|> parseAdd
    <|> parseSub
    <|> parseMul
    <|> parseDiv
    <|> parsePow

runExprParser :: String -> Either String (Expr Integer)
runExprParser text = do
  (rest, x) <- runParser parseExpr text
  if null rest
    then return x
    else Left $ "Excessive input: " ++ rest
