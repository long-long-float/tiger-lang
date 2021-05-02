{-# LANGUAGE OverloadedStrings #-}

module Tiger.Parser where

import Data.Text (Text)
import Data.Void
import Data.Char
import Data.Maybe
import Control.Monad.State
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Map.Strict as M

import Tiger.Symbol

type Parser a = ParsecT Void Text (State SymbolTable) a
type StateM = State SymbolTable

data Declaration
  = TypeDec Symbol Type
  | VarDec Symbol (Maybe Symbol) Expr
  | FunDec Symbol [TyField] (Maybe Symbol) Expr
  deriving (Eq, Ord, Show)

data Type
  = NameTy Symbol
  | RecordTy [TyField]
  | ArrayTy Symbol
  deriving (Eq, Ord, Show)

data TyField
  = Field Symbol Symbol
  deriving (Eq, Ord, Show)

data LValue
  = SimpleVar Symbol
  | FieldVar LValue Symbol
  | SubscriptVar LValue Expr
  deriving (Eq, Ord, Show)

data Expr
  = ValueExpr LValue
  | NilExpr
  | SeqExpr [Expr]
  | IntLit Int
  | StringLit Text
  | UnitLit
  | ArrayCreation Symbol Expr Expr
  | IfExpr Expr Expr (Maybe Expr)
  | WhileExpr Expr Expr
  | ForExpr Symbol Expr Expr Expr
  | LetExpr [Declaration] [Expr]
  | FunctionCall Symbol [Expr]
  | BinaryExpr Expr Text Expr
  | UnaryExpr Text Expr
  | RecordCreation Symbol [(Symbol, Expr)]
  | AssignExpr LValue Expr
  deriving (Eq, Ord, Show)

name2symbol :: Text -> StateM Symbol
name2symbol name = do
  st <- get
  let res = M.lookup name st
  case res of
    Just i -> return $ Symbol name i
    Nothing -> do
      let next = (M.size st) + 1
      put $ M.insert name next st
      return $ Symbol name next

symbol2name :: Symbol -> Text
symbol2name (Symbol s n) = s

sc :: Parser ()
sc = L.space
  space1
  empty
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

identifier :: Parser Symbol
identifier = lexeme $ register $ (\head tail -> T.pack $ head ++ tail) <$>
  (some letterChar) <*> (many $ satisfy isIdChar)
  where
    register :: Parser Text -> Parser Symbol
    register id = do
      name <- lookAhead id
      sym <- lift $ name2symbol name
      (\_ -> sym) <$> id

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

tyannot :: Parser Symbol
tyannot = try $ (\_ ty -> ty) <$> (symbol ":") <*> identifier

data PartialLValue
  = PLArray Expr PartialLValue
  | PLDot   Symbol PartialLValue
  | PLTerm

lvalue :: Parser LValue
lvalue = buildLValue <$> identifier <*> lvalue'
  where
    buildLValue id lv = buildLValue' (SimpleVar id) lv

    buildLValue' id PLTerm = id
    buildLValue' id (PLArray index rest) =
      let left = SubscriptVar id index in
      buildLValue' left rest
    buildLValue' id (PLDot field rest) =
      let left = FieldVar id field in
      buildLValue' left rest

    lvalue' :: Parser PartialLValue
    lvalue' = try ((\_ i _ r -> PLArray i r) <$> symbol "[" <*> expr <*> symbol "]" <*> lvalue')
          <|> try ((\_ f r -> PLDot f r) <$> symbol "." <*> identifier <*> lvalue')
          <|> return PLTerm

expr :: Parser Expr
expr =
      try ifexpr
  <|> try whileexpr
  <|> try forexpr
  <|> try letexpr
  <|> try (AssignExpr <$> lvalue <* symbol ":=" <*> expr)
  <|> try ((\_ exprs _ -> SeqExpr exprs) <$> symbol "(" <*> seqexpr <*> symbol ")")
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
  | PETerm
  deriving (Eq, Ord, Show)

buildOperators ops child = p
  where
    buildBE left PETerm = left
    buildBE left (PE op right rest) =
      let left' = BinaryExpr left op right in
      buildBE left' rest

    p :: Parser Expr
    p = buildBE <$> child <*> p'
      where
        p' :: Parser PartialExpr
        p' = try (PE <$> op <*> child <*> p')
              <|> return PETerm
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
    op = symbol ">=" <|> symbol "<=" <|> symbol "=" <|> symbol "<>" <|> symbol ">" <|> symbol "<"

arith1 :: Parser Expr
arith1 = buildOperators ["+", "-"] arith2

arith2 :: Parser Expr
arith2 = buildOperators ["*", "/"] unary

unary :: Parser Expr
unary = UnaryExpr <$> symbol "-" <*> term <|>
        term

term :: Parser Expr
term = try integerlit
   <|> try ((\_ -> NilExpr) <$> symbol "nil")
   <|> try stringlit
   <|> try ((\_ -> UnitLit) <$> (symbol "(" *> symbol ")"))
   <|> try (FunctionCall <$> identifier <* symbol "(" <*> args <* symbol ")")
   <|> try ((\t e1 _ e2 -> ArrayCreation t e1 e2) <$> identifier <* symbol "[" <*> expr <* symbol "]" <*> symbol "of" <*> expr)
   <|> try recordCreation
   <|> try (ValueExpr <$> lvalue)
   <|> symbol "(" *> expr <* symbol ")"
   where
    args :: Parser [Expr]
    args = (\head rest -> [head] ++ rest) <$> expr <*> many argrest <|>
           return []
    argrest = symbol "," *> expr

integerlit :: Parser Expr
integerlit = IntLit <$> lexeme L.decimal

stringlit :: Parser Expr
stringlit = lexeme $ (\_ str _ -> StringLit $ T.pack $ foldl (++) "" str) <$> char '\"' <*> (many ch) <*> char '\"'
  where
    ch :: Parser [Char]
    ch =  (\s c -> [s] ++ c) <$> char '\\' <*> escCh
      <|> (\c -> [c]) <$> satisfy (\c -> c /= '\"' && isPrint c)

    escCh :: Parser [Char]
    escCh = one <$> char 'n' <|> one <$> char 't' <|> T.unpack <$> string "^C"
        <|> one <$> char '\"'
          -- <|> \ddd ...

    one e = [e]

ifexpr :: Parser Expr
ifexpr =
  -- this is not correct???
  -- IfExpr <$> symbol "if" *> expr <* symbol "then" *> expr <* symbol "else" *> expr
  (\_ e1 _ e2 e3 -> IfExpr e1 e2 e3) <$> symbol "if" <*> expr <*> symbol "then" <*> expr <*> (optional $ symbol "else" *> expr)
  -- IfExpr <$> symbol "if" *> expr <* symbol "then" <*> expr <* symbol "else" <*> expr

whileexpr :: Parser Expr
whileexpr =
  (\_ e1 _ e2 -> WhileExpr e1 e2) <$> symbol "while" <*> expr <*> symbol "do" <*> expr

forexpr :: Parser Expr
forexpr =
  (\_ id _ e1 _ e2 _ e3 -> ForExpr id e1 e2 e3) <$> symbol "for" <*> identifier <*> symbol ":=" <*> expr <*> symbol "to" <*> expr <*> symbol "do" <*> expr

letexpr :: Parser Expr
letexpr = (\_ decs _ exprs _ -> LetExpr decs exprs) <$>
    symbol "let" <*> decs <*> symbol "in" <*> seqexpr <*> symbol "end"
  where
    seqexpr = (\head rest -> [head] ++ rest) <$> expr <*> many seqexprrest <|>
              return []
    seqexprrest = (\_ e -> e) <$> symbol ";" <*> expr

recordCreation :: Parser Expr
recordCreation =
      try ((\id head rest -> RecordCreation id $ [head] ++ rest) <$> identifier <* symbol "{" <*> idexp <*> (many $ symbol "," *> idexp) <* symbol "}")
  <|> (\id -> RecordCreation id []) <$> identifier <* symbol "{" <* symbol "}"
  where
    idexp = (,) <$> identifier <* symbol "=" <*> expr

whole :: Parser Expr
whole = do
    _ <- sc
    prog <- expr
    eof
    return prog

parse :: String -> Text -> Either (ParseErrorBundle Text Void) (Expr, SymbolTable)
parse srcPath src = do
  let st = runParserT whole srcPath src
  case runState st M.empty of
    (Left err, _) -> Left err
    (Right ast, symbols) -> Right (ast, symbols)

