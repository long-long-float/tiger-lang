{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import Data.Int (Int)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

type Parser a = Parsec Void Text a

data Expr
  = Int Int
  | Binary Expr Text Expr
  | Id Text
  | Decs [Expr]
  | TypeDec Expr Expr
  | ArrayTy Expr
  | VarDec Expr Expr
  deriving (Eq, Ord, Show)

sc :: Parser ()
sc = L.space
  space1
  empty
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

integer :: Parser Expr
integer = Int <$> lexeme L.decimal

-- TODO: fix
identifier :: Parser Expr
-- identifier = lexeme $ fmap (\chs -> Id $ T.pack chs) $ some letterChar
identifier = (\chs -> Id $ T.pack chs) <$> (lexeme $ some letterChar)

decs :: Parser Expr
decs = Decs <$> many dec

dec :: Parser Expr
dec = tydec <|> vardec -- <|> fundec

tydec :: Parser Expr
tydec = (\_ name _ ty -> TypeDec name ty) <$> (symbol "type") <*> identifier <*> (symbol "=") <*> ty

ty :: Parser Expr
ty = identifier <|>
  (\_ _ ty -> ArrayTy ty) <$> (symbol "array") <*> (symbol "of") <*> identifier

vardec :: Parser Expr
vardec = (\_ id _ ex -> VarDec id ex) <$> (symbol "var") <*> identifier <*> (symbol ":=") <*> expr

atom :: Parser Expr
atom = do
    symbol "("
    x <- expr
    symbol ")"
    return x
  <|> integer

expr :: Parser Expr
expr =
  try (Binary <$> atom <*> (symbol "+") <*> atom) <|>
  (Binary <$> atom <*> (symbol "-") <*> atom)

whole :: Parser Expr
whole = do
    ex <- decs
    eof
    return ex

main :: IO ()
main = do
  putStrLn "Enter expression:"
  s <- getLine
  case parse whole "stdin" (T.pack s) of
    Left err -> print err
    Right x -> print x

