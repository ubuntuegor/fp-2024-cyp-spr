module Parser (parseProgram, Op (..), Expr (..), Stmt (..), Func (..), Program (..)) where

import Control.Monad.Combinators.Expr
  ( Operator (InfixL),
    makeExprParser,
  )
import Data.Void (Void)
import Text.Megaparsec
  ( MonadParsec (try),
    Parsec,
    between,
    choice,
    empty,
    eof,
    many,
    optional,
    sepBy,
    (<|>),
  )
import Text.Megaparsec.Char (alphaNumChar, letterChar, space1)
import qualified Text.Megaparsec.Char.Lexer as L

data Op = Add | Sub | Mul | Div | Mod | Eq | Ne | Gt | Ge | Lt | Le | And | Or
  deriving (Show, Eq)

data Expr = Const Int | Var String | Binop Op Expr Expr | Call String [Expr]
  deriving (Show, Eq)

data Stmt = Asgn String Expr | Expr' Expr | Write Expr | Read String | While Expr Stmt | If Expr Stmt (Maybe Stmt) | Skip | Seq [Stmt]
  deriving (Show, Eq)

data Func = Func String [String] Stmt
  deriving (Show, Eq)

data Program = Program [Func] Stmt
  deriving (Show, Eq)

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parseConst :: Parser Int
parseConst = lexeme $ L.signed sc L.decimal

parseIdent :: Parser String
parseIdent = lexeme $ do
  h <- letterChar
  t <- many alphaNumChar
  return (h : t)

parseCall :: Parser Expr
parseCall = try $ do
  name <- parseIdent
  args <- between (symbol "(") (symbol ")") (sepBy parseExpr (symbol ","))
  return $ Call name args

parseTerm :: Parser Expr
parseTerm =
  choice
    [ Const <$> parseConst,
      parseCall,
      Var <$> parseIdent,
      between (symbol "(") (symbol ")") parseExpr
    ]

parseExpr :: Parser Expr
parseExpr =
  makeExprParser
    parseTerm
    [ [ binop Mul "*",
        binop Div "/",
        binop Mod "%"
      ],
      [ binop Add "+",
        binop Sub "-"
      ],
      [ binop Eq "==",
        binop Ne "!=",
        binop Gt ">",
        binop Ge ">=",
        binop Lt "<",
        binop Le "<="
      ],
      [binop And "&&"],
      [binop Or "||"]
    ]
  where
    binop op str = InfixL (Binop op <$ symbol str)

parseAsgn :: Parser Stmt
parseAsgn = try $ do
  name <- parseIdent
  _ <- symbol "="
  Asgn name <$> parseExpr

parseExpr' :: Parser Stmt
parseExpr' = Expr' <$> parseExpr

parseWrite :: Parser Stmt
parseWrite = do
  _ <- symbol "write"
  Write <$> parseExpr

parseRead :: Parser Stmt
parseRead = do
  _ <- symbol "read"
  Read <$> parseIdent

parseWhile :: Parser Stmt
parseWhile = do
  _ <- symbol "while"
  cond <- between (symbol "(") (symbol ")") parseExpr
  body <- between (symbol "{") (symbol "}") parseStmt
  return $ While cond body

parseIf :: Parser Stmt
parseIf = do
  _ <- symbol "if"
  cond <- between (symbol "(") (symbol ")") parseExpr
  body <- between (symbol "{") (symbol "}") parseStmt
  elseBody <- optional $ do
    _ <- symbol "else"
    parseIf <|> between (symbol "{") (symbol "}") parseStmt
  return $ If cond body elseBody

parseSkip :: Parser Stmt
parseSkip = Skip <$ symbol "skip"

parseStmt :: Parser Stmt
parseStmt =
  Seq
    <$> sepBy
      ( choice
          [ parseAsgn,
            parseSkip,
            parseIf,
            parseWhile,
            parseRead,
            parseWrite,
            parseExpr'
          ]
      )
      (symbol ";")

parseFunc :: Parser Func
parseFunc = do
  _ <- symbol "function"
  name <- parseIdent
  args <- between (symbol "(") (symbol ")") (sepBy parseIdent (symbol ","))
  body <- between (symbol "{") (symbol "}") parseStmt
  return $ Func name args body

parseProgram :: Parser Program
parseProgram = Program <$> (sc *> many parseFunc) <*> parseStmt <* eof
