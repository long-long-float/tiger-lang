{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (id)
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
import qualified Control.Monad.Catch as C
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import qualified Data.Text.IO as IO
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Safe as S

import qualified Types as Ty

data Symbol = Symbol { name :: Text, id :: Int }
  deriving (Eq, Ord, Show)
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

type Table = IM.IntMap

type Parser a = ParsecT Void Text (State SymbolTable) a

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
  | ArrayCreation Symbol Expr Expr
  | IfExpr Expr Expr Expr
  | LetExpr [Declaration] [Expr]
  | FunctionCall Symbol [Expr]
  | BinaryExpr Expr Text Expr
  | UnaryExpr Text Expr
  deriving (Eq, Ord, Show)

data EnvEntry
  = VarEntry Ty.Type
  | FunEntry { formals :: [Ty.Type], result :: Ty.Type }
  deriving (Eq, Ord, Show)

type VEnv = Table EnvEntry
type TEnv = Table Ty.Type
type EnvStateT m a = StateT (VEnv, TEnv) m a

data ExprTy = ExprTy { typ :: Ty.Type }
  deriving (Eq, Ord, Show)

data TypeException = TypeException Text deriving (Show)
instance C.Exception TypeException
data UnimplementedException = UnimplementedException Text deriving (Show)
instance C.Exception UnimplementedException

fromAbstType :: (C.MonadThrow m) => Type -> EnvStateT m Ty.Type
fromAbstType (NameTy sym) = getType sym
fromAbstType (RecordTy fields) = C.throwM $ UnimplementedException "fromAbstType"
fromAbstType (ArrayTy inner) = do
  inner <- getType inner
  return $ Ty.Array inner

(-+-) :: Text -> Text -> Text
a -+- b = T.append a b

(-+$) :: Text -> String -> Text
a -+$ b = T.append a (T.pack b)

quoteTy :: Ty.Type -> Text
quoteTy ty = "'" -+$ (show ty) -+- "'"

expectsType :: (C.MonadThrow m) => ExprTy -> Ty.Type -> m ()
expectsType ty expected = do
  if typ ty /= expected then
    C.throwM $ TypeException $ "Couldn't match expected type " -+- (quoteTy $ typ ty) -+- " with actual type " -+- (quoteTy expected)
  else
    return ()

getType :: (C.MonadThrow m) => Symbol -> EnvStateT m Ty.Type
getType sym = do
  (_, te) <- get
  case IM.lookup (id sym) te of
    Just ty -> return ty
    Nothing ->  C.throwM $ TypeException $ "Undefined type: " -+- (name sym)

getVar :: (C.MonadThrow m) => Symbol -> EnvStateT m EnvEntry
getVar sym = do
  (ve, _) <- get
  case IM.lookup (id sym) ve of
    Just entry -> return entry
    Nothing ->  C.throwM $ TypeException $ "Undefined variable: " -+- (name sym)

transLValue :: (C.MonadThrow m) => LValue -> EnvStateT m ExprTy
transLValue (SimpleVar sym) = do
  (ve, te) <- get
  case IM.lookup (id sym) ve of
    Just (VarEntry var) -> return $ ExprTy var
    Nothing ->  C.throwM $ TypeException $ "Undefined variable: " -+- (name sym)
transLValue _ = C.throwM $ TypeException "undef@lvalue"

transExpr :: (C.MonadThrow m) => Expr -> EnvStateT m ExprTy
transExpr (ValueExpr lv) = transLValue lv
transExpr NilExpr = return $ ExprTy Ty.Nil
transExpr (SeqExpr exprs) = do
  types <- mapM transExpr exprs
  return $ S.lastDef (ExprTy Ty.Unit) types
transExpr (ArrayCreation inner n v) = do
  inner <- getType inner
  n <- transExpr n
  v <- transExpr v
  expectsType n Ty.Int
  expectsType v inner
  return $ ExprTy $ Ty.Array inner
transExpr (IfExpr cond t f) = do
  cond <- transExpr cond
  t <- transExpr t
  f <- transExpr f
  expectsType cond Ty.Int
  expectsType t (typ f)
  return t
transExpr (LetExpr decs exprs) = do
  env <- get
  mapM transDecs decs
  types <- mapM transExpr exprs
  put env
  return $ S.lastDef (ExprTy Ty.Unit) types
  where
    transDecs :: (C.MonadThrow m) => Declaration -> EnvStateT m ()
    transDecs (TypeDec sym ty) = do
      (ve, te) <- get
      ty <- fromAbstType ty
      let te' = IM.insert (id sym) ty te
      put (ve, te')
    transDecs (VarDec sym ty init) = do
      (ve, te) <- get
      init <- transExpr init
      ty' <- case ty of
                Just ty' -> getType ty'
                Nothing -> return $ typ init
      expectsType init ty'
      let ve' = IM.insert (id sym) (VarEntry ty') ve
      put (ve', te)
    transDecs (FunDec sym fields ty body) = do
      (ve, te) <- get
      fieldTypes <- mapM insertField fields
      actualRet <- transExpr body
      ret <- case ty of
              Just ret -> getType ret
              Nothing -> return $ typ actualRet
      expectsType actualRet ret
      let ve' = IM.insert (id sym) (FunEntry fieldTypes ret) ve
      put (ve', te)

    insertField :: (C.MonadThrow m) => TyField -> EnvStateT m Ty.Type
    insertField (Field var ty) = do
      (ve, te) <- get
      ty <- getType ty
      let ve' = IM.insert (id var) (VarEntry ty) ve
      put (ve', te)
      return ty
transExpr (FunctionCall sym exprs) = do
  callee <- getVar sym
  case callee of
    VarEntry _ -> C.throwM $ TypeException $ "'" -+- (name sym) -+- "' is not a function"
    FunEntry fields ret -> do
      args <- mapM transExpr exprs
      if (length args) /= (length fields) then
        C.throwM $ TypeException "argument count mismatch"
      else
        return $ ExprTy ret
transExpr (IntLit _) = return $ ExprTy Ty.Int
transExpr (StringLit _) = return $ ExprTy Ty.String
transExpr (BinaryExpr left _ right) = do
  left <- transExpr left
  right <- transExpr right
  expectsType left Ty.Int
  expectsType right Ty.Int
  return $ ExprTy Ty.Int
transExpr (UnaryExpr _ e) = do
  e <- transExpr e
  expectsType e Ty.Int
  return $ ExprTy Ty.Int

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

getSymbolId :: Text -> SymbolTable -> Int
getSymbolId name st =
  fromMaybe 0 (M.lookup name st)

main :: IO ()
main = do
  args <- getArgs
  let srcPath = args !! 0
  s <- IO.readFile srcPath
  let st = runParserT whole srcPath s
  case runState st M.empty of
    (Left err, _) -> putStr $ errorBundlePretty err
    (Right ast, symbols) -> do
      print ast

      let te = IM.fromList [(getSymbolId "int" symbols, Ty.Int), (getSymbolId "string" symbols, Ty.String)]
      let typed = evalStateT (transExpr ast) (IM.empty, te) :: Either C.SomeException ExprTy
      print typed

