{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import Data.Int (Int)
import Data.Void
import Data.Maybe
import Data.Char
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import Control.Monad.State
import System.Environment (getArgs)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as IO
import qualified Text.Megaparsec.Char.Lexer as L

data Symbol = Symbol Text Int
type SymbolTable = M.Map Text Int
type StateM = State SymbolTable

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

type Parser a = ParsecT Void Text (State SymbolTable) a

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
  = SimpleVar Identifier
  | FieldVar LValue Identifier
  | SubscriptVar LValue Expr
  deriving (Eq, Ord, Show)

data Expr
  = ValueExpr LValue
  | NilExpr
  | SeqExpr [Expr]
  | IntLit Int
  | StringLit Text
  | ArrayCreation Identifier Expr Expr
  | IfExpr Expr Expr Expr
  | LetExpr [Declaration] [Expr]
  | FunctionCall Identifier [Expr]
  | BinaryExpr Expr Text Expr
  | UnaryExpr Text Expr
  deriving (Eq, Ord, Show)

data EnvEntry
  = VarEntry Type
  | FunEntry { formals :: [Type], result :: Type }
data Table a = Table a

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
identifier = lexeme $ register $ (\head tail -> Id $ T.pack $ head ++ tail) <$>
  (some letterChar) <*> (many $ satisfy isIdChar)
  where
    register :: Parser Identifier -> Parser Identifier
    register id = do
      (Id name) <- lookAhead id
      lift $ name2symbol name
      id

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

data PartialLValue
  = PLArray Expr PartialLValue
  | PLDot   Identifier PartialLValue
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
      try (dbg "if" ifexpr)
  <|> try (dbg "let" letexpr)
  <|> try (dbg "sequence" ((\_ exprs _ -> SeqExpr exprs) <$> symbol "(" <*> seqexpr <*> symbol ")"))
  <|> try (dbg "boolop" boolop1)
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
   <|> try (dbg "nil" ((\_ -> NilExpr) <$> symbol "nil"))
   <|> try (dbg "string" stringlit)
   <|> try (FunctionCall <$> identifier <* symbol "(" <*> args <* symbol ")")
   <|> try (dbg "arraycreation" ((\t e1 _ e2 -> ArrayCreation t e1 e2) <$> identifier <* symbol "[" <*> expr <* symbol "]" <*> symbol "of" <*> expr))
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
  (\_ e1 _ e2 _ e3 -> IfExpr e1 e2 e3) <$> symbol "if" <*> expr <*> symbol "then" <*> expr <*> symbol "else" <*> expr
  -- IfExpr <$> symbol "if" *> expr <* symbol "then" <*> expr <* symbol "else" <*> expr

letexpr :: Parser Expr
letexpr = (\_ decs _ exprs _ -> LetExpr decs exprs) <$>
    symbol "let" <*> (dbg "decs" decs) <*> symbol "in" <*> seqexpr <*> symbol "end"
  where
    seqexpr = (\head rest -> [head] ++ rest) <$> expr <*> many seqexprrest <|>
              return []
    seqexprrest = (\_ e -> e) <$> symbol ";" <*> expr

whole :: Parser Expr
whole = do
    _ <- sc
    prog <- expr
    eof
    return prog

main :: IO ()
main = do
  args <- getArgs
  let srcPath = args !! 0
  s <- IO.readFile srcPath
  let st = runParserT whole srcPath s
  case runState st M.empty of
    (Left err, _) -> putStr $ errorBundlePretty err
    (Right x, symbols) -> do
      print x
      print symbols

