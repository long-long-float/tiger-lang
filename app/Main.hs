{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import Data.Int (Int)
import Data.Void
import Data.Maybe
import Data.Char
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
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

data LValue
  = Variable Identifier
  deriving (Eq, Ord, Show)

data Expr
  = ValueExpr LValue
  | NilExpr
  | SeqExpr [Expr]
  | IntLit Int
  | StringLit Text
  | IfExpr Expr Expr Expr
  | LetExpr [Declaration] [Expr]
  | BinaryExpr Expr Text Expr
  | UnaryExpr Text Expr
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
vardec = (\_ id tyannot _ ex -> VarDec id tyannot ex) <$>
  (symbol "var") <*> identifier <*> (optional tyannot) <*> (symbol ":=") <*> expr

fundec :: Parser Declaration
fundec = (\_ id _ params _ tyannot _ body -> FunDec id params tyannot body) <$>
         (symbol "function") <*> identifier <*> (symbol "(") <*> tyfields <*> (symbol ")") <*> (optional tyannot) <*> (symbol "=") <*> expr

tyannot :: Parser Identifier
tyannot = try $ (\_ ty -> ty) <$> (symbol ":") <*> identifier

-- TODO: add record and array
lvalue :: Parser LValue
lvalue =
  try (Variable <$> identifier)

expr :: Parser Expr
expr =
      try (dbg "if" ifexpr)
  <|> try (dbg "let" letexpr)
  <|> try (dbg "sequence" ((\_ exprs _ -> SeqExpr exprs) <$> symbol "(" <*> seqexpr <*> symbol ")"))
  <|> try boolop1
  where
    seqexpr = (\head rest -> [head] ++ rest) <$> expr <*> some seqexprrest
    seqexprrest = (\_ e -> e) <$> symbol ";" <*> expr

-- arith1 :: Parser Expr
-- arith1 = try (BinaryExpr <$> arith2 <*> op <*> expr)
--      <|> arith2
--   where
--     op = symbol "+" <|> symbol "-"

data PartialExpr
  = PE Text Expr PartialExpr
  | Term
  deriving (Eq, Ord, Show)

buildBE left Term = left
buildBE left (PE op right rest) =
  let left' = BinaryExpr left op right in
  buildBE left' rest

buildOperators ops child = p
  where
    p :: Parser Expr
    p = buildBE <$> child <*> p'
      where
        p' :: Parser PartialExpr
        p' = try (PE <$> op <*> child <*> p')
              <|> return Term
        ops2p [op] = symbol op
        ops2p (x:xs) = symbol x <|> ops2p xs
        op = ops2p ops

boolop1 :: Parser Expr
boolop1 = buildOperators ["&"] boolop2

boolop2 :: Parser Expr
boolop2 = buildOperators ["|"] comp1

comp1 :: Parser Expr
comp1 = try (BinaryExpr <$> arith1 <*> op <*> arith1) <|>
        arith1
  where
    op = symbol "=" <|> symbol "<>" <|> symbol ">" <|> symbol "<"
     <|> symbol ">=" <|> symbol "<="

arith1 :: Parser Expr
arith1 = buildOperators ["+", "-"] arith2

arith2 :: Parser Expr
arith2 = buildOperators ["*", "/"] unary

unary :: Parser Expr
unary = UnaryExpr <$> symbol "-" <*> term <|>
        term

term :: Parser Expr
term = try integerlit
   <|> try (dbg "nil" ((\_ -> NilExpr) <$> symbol "nil"))
   <|> try (dbg "string" stringlit)
   <|> try (ValueExpr <$> lvalue)
   <|> symbol "(" *> expr <* symbol ")"

integerlit :: Parser Expr
integerlit = IntLit <$> lexeme L.decimal

stringlit :: Parser Expr
stringlit = lexeme $ (\_ str _ -> StringLit $ T.pack $ foldl (++) "" str) <$> char '\"' <*> (many (dbg "ch" ch)) <*> char '\"'
  where
    ch :: Parser [Char]
    ch =  (\c -> [c]) <$> satisfy (\c -> c /= '\"' && isPrint c)
      <|> (\s c -> [s] ++ c) <$> char '\\' <*> escCh

    escCh :: Parser [Char]
    escCh = one <$> char 'n' <|> one <$> char 't' <|> T.unpack <$> string "^C"
          -- <|> \ddd ...

    one e = [e]

ifexpr :: Parser Expr
ifexpr =
  -- this is not correct???
  -- IfExpr <$> symbol "if" *> expr <* symbol "then" *> expr <* symbol "else" *> expr
  (\e1 e2 e3 -> IfExpr) <$> symbol "if" *> expr <* symbol "then" *> expr <* symbol "else" *> expr

letexpr :: Parser Expr
letexpr = (\_ decs _ exprs _ -> LetExpr decs exprs) <$>
    symbol "let" <*> decs <*> symbol "in" <*> seqexpr <*> symbol "end"
  where
    seqexpr = (\head rest -> [head] ++ rest) <$> expr <*> many seqexprrest <|>
              return []
    seqexprrest = (\_ e -> e) <$> symbol ";" <*> expr

-- expr :: Parser Expr
-- expr =
--   try (Binary <$> atom <*> (symbol "+") <*> atom) <|>
--   (Binary <$> atom <*> (symbol "-") <*> atom)

whole :: Parser Expr
whole = do
    prog <- expr
    eof
    return prog

main :: IO ()
main = do
  putStrLn "Enter expression:"
  s <- getLine
  case parse whole "stdin" (T.pack s) of
    Left err -> print err
    Right x -> print x

