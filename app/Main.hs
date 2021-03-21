{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import Data.Int (Int)
import Data.Void
import Data.Maybe
import Data.Char
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

type Parser a = Parsec Void Text a

newtype Identifier = Id Text
  deriving (Eq, Ord, Show)

data Declaration
  = TypeDec Identifier Type
  | VarDec Identifier (Maybe Identifier) Expr
  | FunDec Identifier [TyField] (Maybe Identifier) Expr
  deriving (Eq, Ord, Show)

data Type
  = NameTy Identifier
  | RecordTy [TyField]
  | ArrayTy Identifier
  deriving (Eq, Ord, Show)

data TyField
  = Field Identifier Identifier
  deriving (Eq, Ord, Show)

data Expr
  = Int Int
  | Binary Expr Text Expr
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

identifier :: Parser Identifier
-- identifier = (\head tail -> Id $ T.pack $ head ++ tail) <$>
identifier = lexeme $ (\head tail -> Id $ T.pack head) <$>
  (some letterChar) <*> (many $ satisfy isIdChar)
  where
    isIdChar x = (isAlphaNum x) || x == '_'

decs :: Parser [Declaration]
decs = many dec

dec :: Parser Declaration
dec = tydec <|> vardec <|> fundec

tydec :: Parser Declaration
tydec = (\_ name _ ty -> TypeDec name ty) <$> (symbol "type") <*> identifier <*> (symbol "=") <*> ty

ty :: Parser Type
ty =
  (\_ _ ty -> ArrayTy ty) <$> (symbol "array") <*> (symbol "of") <*> identifier <|>
  (\_ fields _ -> RecordTy fields) <$> (symbol "{") <*> tyfields <*> (symbol "}") <|>
  NameTy <$> identifier

tyfields :: Parser [TyField]
tyfields =
  (\head rest -> [head] ++ rest) <$> tyfield <*> many tfrest <|>
  return []
  where
    tfrest = (\_ tf -> tf) <$> (symbol ",") <*> tyfield

tyfield :: Parser TyField
tyfield = (\id _ tyid -> Field id tyid) <$> identifier <*> (symbol ":") <*> identifier

vardec :: Parser Declaration
vardec = (\_ id tyannot _ ex -> VarDec id tyannot ex) <$> (symbol "var") <*> identifier <*> (optional tyannot) <*> (symbol ":=") <*> expr

fundec :: Parser Declaration
fundec = (\_ id _ params _ tyannot _ body -> FunDec id params tyannot body) <$> (symbol "function") <*> identifier <*> (symbol "(") <*> tyfields <*> (symbol ")") <*> (optional tyannot) <*> (symbol "=") <*> expr

tyannot :: Parser Identifier
tyannot = try $ (\_ ty -> ty) <$> (symbol ":") <*> identifier

expr :: Parser Expr
expr =
  try (Binary <$> atom <*> (symbol "+") <*> atom) <|>
  (Binary <$> atom <*> (symbol "-") <*> atom)

atom :: Parser Expr
atom = do
    symbol "("
    x <- expr
    symbol ")"
    return x
  <|> integer

whole :: Parser [Declaration]
whole = do
    d <- decs
    eof
    return d

main :: IO ()
main = do
  putStrLn "Enter expression:"
  s <- getLine
  case parse whole "stdin" (T.pack s) of
    Left err -> print err
    Right x -> print x

