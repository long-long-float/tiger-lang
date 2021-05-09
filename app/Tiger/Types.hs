{-# LANGUAGE OverloadedStrings #-}

module Tiger.Types where

import Prelude hiding (id)
import Data.Text (Text)
import Data.Maybe
import Data.List
import Control.Monad.State
import qualified Control.Monad.Catch as C
import qualified Data.IntMap.Strict as IM
import qualified Safe
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Tiger.Symbol hiding (name, id)
import qualified Tiger.Symbol as S
import qualified Tiger.Parser as P

data Type
  = Int
  | String
  | Record [(Symbol, TypeWithName)]
  | Array TypeWithName -- TODO: Add unique
  | Nil
  | Unit
  | Name
  | Undetermined -- This is used for recursive types
  deriving (Eq, Ord, Show)

data TypeWithName = TypeWithName { ty :: Type, name :: Symbol }
  deriving (Eq, Ord, Show)

type Table = IM.IntMap

data EnvEntry
  = VarEntry TypeWithName
  | FunEntry { formals :: [TypeWithName], result :: TypeWithName }
  deriving (Eq, Ord, Show)

type VEnv = Table EnvEntry
type TEnv = Table TypeWithName
type EnvStateT m a = StateT (VEnv, TEnv) m a

formatEnv :: (VEnv, TEnv) -> Text
formatEnv (ve, te) = "Var Env:\n  " -+- (formatTable ve) -+- "\nType Env:\n  " -+- (formatTable te)
  where
    formatTable table = T.intercalate "\n  " $ map (\(k, v) -> (show k) $+- " = " -+$ (show v)) $ IM.toList table

data ExprTy = ExprTy { ty_ :: TypeWithName }
  deriving (Eq, Ord, Show)

typeOf :: ExprTy -> Type
typeOf (ExprTy (TypeWithName ty _)) = ty

anon :: Type -> TypeWithName
anon ty = TypeWithName ty emptySymbol

data TypeException = TypeException Text deriving (Show)
instance C.Exception TypeException
data UnimplementedException = UnimplementedException Text deriving (Show)
instance C.Exception UnimplementedException

dumpEnv :: (C.MonadThrow m) => EnvStateT m TypeWithName
dumpEnv = do
  env' <- get
  C.throwM $ TypeException $ formatEnv env'

getSymbolId :: Text -> SymbolTable -> Int
getSymbolId name st =
  fromMaybe 0 (M.lookup name st)

defaultEnv :: SymbolTable -> (VEnv, TEnv)
defaultEnv symbols = (ve, te)
  where
    ve = IM.fromList [
      (s "print", FunEntry [string] unit),
      (s "flush", FunEntry [] unit),
      (s "getchar", FunEntry [] string),
      (s "ord", FunEntry [string] int),
      (s "chr", FunEntry [int] string),
      (s "size", FunEntry [string] int),
      (s "substring", FunEntry [string, int, int] string),
      (s "concat", FunEntry [string, string] string),
      (s "not", FunEntry [int] int),
      (s "exit", FunEntry [int] unit)]
    te = IM.fromList [
      (s "int", int),
      (s "string", string)]
    s sym = getSymbolId sym symbols
    string = anon String
    int = anon Int
    unit = anon Unit

fromAbstType :: (C.MonadThrow m) => P.Type -> EnvStateT m TypeWithName
fromAbstType (P.NameTy sym) = getType sym
fromAbstType (P.RecordTy fields) = do
  pairs <- flip mapM fields $ \(P.Field id ty) -> do
    ty <- getType ty
    return (id, ty)
  return $ TypeWithName (Record pairs) emptySymbol
fromAbstType (P.ArrayTy inner) = do
  inner <- getType inner
  return $ TypeWithName (Array inner) emptySymbol

throwIf :: (C.MonadThrow m, C.Exception e) => Bool -> e -> m ()
throwIf cond ex = do
  if cond then
    C.throwM ex
  else
    return ()

(-+-) :: Text -> Text -> Text
a -+- b = T.append a b

(-+$) :: Text -> String -> Text
a -+$ b = T.append a (T.pack b)

($+-) :: String -> Text -> Text
a $+- b = T.append (T.pack a) b

quoteTy :: Type -> Text
quoteTy ty = "'" -+$ (show ty) -+- "'"

quote :: (Show s) => s -> Text
quote s = "'" -+$ show s -+- "'"

returnTy :: (C.MonadThrow m) => Type -> EnvStateT m ExprTy
returnTy ty = return $ ExprTy $ TypeWithName ty emptySymbol

eqType :: (C.MonadThrow m) => TypeWithName -> TypeWithName -> EnvStateT m Bool
eqType (TypeWithName t1 id1) (TypeWithName t2 id2) = do
  t1 <- resolveType t1 id1
  t2 <- resolveType t2 id2
  return $ (eqName id1 id2) && (eqType' t1 t2)
  where
    resolveType :: (C.MonadThrow m) => Type -> Symbol -> EnvStateT m Type
    resolveType Name sym = do
      ty' <- getType sym
      return $ ty ty'
    resolveType ty _ = return ty

    eqName (Symbol "" _) _ = True
    eqName _ (Symbol "" _) = True
    eqName n1 n2 = n1 == n2

    eqType' Nil (Record _) = True
    eqType' (Record _) Nil = True
    eqType' t1 t2 = t1 == t2

expectsType :: (C.MonadThrow m) => ExprTy -> TypeWithName -> EnvStateT m ()
expectsType ty expected = do
  eqt <- eqType (ty_ ty) expected
  if not eqt then do
    dumpEnv
    C.throwM $ TypeException $ "Couldn't match expected type " -+- (quote expected) -+- " with actual type " -+- (quote $ ty_ ty)
  else
    return ()

getType :: (C.MonadThrow m) => Symbol -> EnvStateT m TypeWithName
getType sym = getType' sym []
  where
    getType' :: (C.MonadThrow m) => Symbol -> [Symbol] -> EnvStateT m TypeWithName
    getType' sym path = do
      throwIf (elem sym path) (TypeException $ "circular reference: " -+- (S.name sym))
      (_, te) <- get
      case IM.lookup (S.id sym) te of
        Just (TypeWithName Name sym') -> getType' sym' $ sym:path
        Just (TypeWithName Undetermined sym') -> return $ TypeWithName Name sym'
        Just ty -> return ty
        Nothing -> C.throwM $ TypeException $ "Undefined type: " -+- (S.name sym)

insertType :: (C.MonadThrow m) => Symbol -> TypeWithName -> EnvStateT m ()
insertType name ty = do
  (ve, te) <- get
  let te' = IM.insert (S.id name) ty te
  put (ve, te')
  return ()

getVar :: (C.MonadThrow m) => Symbol -> EnvStateT m EnvEntry
getVar sym = do
  (ve, _) <- get
  case IM.lookup (S.id sym) ve of
    Just entry -> return entry
    Nothing ->  C.throwM $ TypeException $ "Undefined variable or function: " -+- (S.name sym)

insertVar :: (C.MonadThrow m) => Symbol -> TypeWithName -> EnvStateT m ()
insertVar var ty = do
  (ve, te) <- get
  let ve' = IM.insert (S.id var) (VarEntry ty) ve
  put (ve', te)
  return ()

transLValue :: (C.MonadThrow m) => P.LValue -> EnvStateT m ExprTy
transLValue (P.SimpleVar sym) = do
  v <- getVar sym
  case v of
    VarEntry var -> return $ ExprTy var
    FunEntry _ _ -> C.throwM $ TypeException $ "'" -+- (S.name sym) -+- "' must be a variable"
transLValue (P.FieldVar rlv field) = do
  rlv <- transLValue rlv
  case typeOf rlv of
    Record pairs -> do
      case find (\(sym, ty) -> sym == field) pairs of
        Just (_, ty) -> return $ ExprTy ty
        Nothing -> C.throwM $ TypeException $ "The field '" -+- (S.name field) -+- "' is not defined"
    _ -> C.throwM $ TypeException $ "The value of '" -+- (S.name field) -+- "' must be a record"
transLValue (P.SubscriptVar rlv idx) = do
  rlv' <- transLValue rlv
  case typeOf rlv' of
    Array ty -> return $ ExprTy ty
    _ -> C.throwM $ TypeException $ "The value '" -+$ (show rlv) -+- "' must be a array"

transExpr :: (C.MonadThrow m) => P.Expr -> EnvStateT m ExprTy
transExpr (P.ValueExpr lv) = transLValue lv
transExpr P.NilExpr = returnTy Nil
transExpr (P.SeqExpr exprs) = do
  types <- mapM transExpr exprs
  return $ Safe.lastDef (ExprTy $ anon Unit) types
transExpr (P.ArrayCreation sym n v) = do
  ty' <- getType sym
  case ty ty' of
    Array inner -> do
      n <- transExpr n
      v <- transExpr v
      expectsType n $ anon Int
      expectsType v inner
      return $ ExprTy ty'
    _ -> C.throwM $ TypeException $ (show ty') $+- " must be array"
transExpr (P.IfExpr cond t f) = do
  cond <- transExpr cond
  t <- transExpr t
  expectsType cond $ anon Int
  case f of
    Just f -> do
      f <- transExpr f
      expectsType t (ty_ f)
      return t
    Nothing -> do
      expectsType t $ anon Unit
      returnTy Unit
transExpr (P.WhileExpr cond body) = do
  cond <- transExpr cond
  body <- transExpr body
  expectsType cond $ anon Int
  expectsType body $ anon Unit
  returnTy Unit
transExpr (P.ForExpr id begin end body) = do
  begin <- transExpr begin
  end <- transExpr end
  expectsType begin $ anon Int
  expectsType end $ anon Int
  (ve, te) <- get
  insertVar id $ anon Int
  body <- transExpr body
  expectsType body $ anon Unit
  put (ve, te)
  returnTy Unit
transExpr (P.LetExpr decs exprs) = do
  env <- get
  mapM scanDecs decs
  mapM resolveDecs decs
  mapM transDecs decs
  types <- mapM transExpr exprs
  put env
  return $ Safe.lastDef (ExprTy $ anon Unit) types
  where
    scanDecs :: (C.MonadThrow m) => P.Declaration -> EnvStateT m ()
    scanDecs (P.TypeDec sym aty) = do
      -- insertType sym $ TypeWithName Name sym
      insertType sym $ TypeWithName Undetermined sym
    scanDecs (P.VarDec sym ty init) = do
      return ()
    scanDecs (P.FunDec sym fields ty body) = do
      fieldTypes <- mapM (\(P.Field _ ty) -> getTypeOrName ty) fields
      ret <- case ty of
                Just ret -> getTypeOrName ret
                Nothing -> return $ anon Unit
      (ve, te) <- get
      let ve' = IM.insert (S.id sym) (FunEntry fieldTypes ret) ve
      put (ve', te)
    getTypeOrName :: (C.MonadThrow m) => Symbol -> EnvStateT m TypeWithName
    getTypeOrName sym = do
      (_, te) <- get
      case IM.lookup (S.id sym) te of
        Just (TypeWithName Undetermined sym') -> return $ TypeWithName Name sym'
        Just ty -> return ty
        Nothing -> return $ TypeWithName Name sym

    resolveDecs :: (C.MonadThrow m) => P.Declaration -> EnvStateT m ()
    resolveDecs (P.TypeDec sym aty) = do
      (ve, te) <- get
      (TypeWithName ty' sym') <- fromAbstType aty
      let sym'' = if sym' == emptySymbol then
                    sym
                  else
                    sym'
      let te' = IM.insert (S.id sym) (TypeWithName ty' sym'') te
      put (ve, te')
    resolveDecs (P.VarDec sym ty init) = do
      return ()
    resolveDecs (P.FunDec sym fields ty body) = do
      (ve, te) <- get
      fieldTypes <- mapM (\(P.Field _ ty) -> getType ty) fields
      ret <- case ty of
                  Just ret -> do
                    ret <- getType ret
                    return ret
                  Nothing -> do
                    return $ anon Unit
      put (ve, te)
      let ve' = IM.insert (S.id sym) (FunEntry fieldTypes ret) ve
      put (ve', te)

    transDecs :: (C.MonadThrow m) => P.Declaration -> EnvStateT m ()
    transDecs (P.TypeDec sym aty) = do
      return ()
    transDecs (P.VarDec sym ty init) = do
      (ve, te) <- get
      init <- transExpr init
      ty' <- case ty of
                Just ty' -> getType ty'
                Nothing -> do
                  throwIf ((typeOf init) == Nil) (TypeException "initializing nil not constrained by record type is not accepted")
                  return $ ty_ init
      expectsType init ty'
      let ve' = IM.insert (S.id sym) (VarEntry ty') ve
      put (ve', te)
    transDecs (P.FunDec sym fields ty body) = do
      (ve, te) <- get
      fieldTypes <- mapM insertField fields
      actualRet <- transExpr body
      ret <- case ty of
        Just ret -> do
          ret <- getType ret
          expectsType actualRet ret
          return ret
        Nothing -> do
          throwIf ((typeOf actualRet) /= Unit) (TypeException "Procedure cannot return value")
          return $ anon Unit
      put (ve, te)
      let ve' = IM.insert (S.id sym) (FunEntry fieldTypes ret) ve
      put (ve', te)

    insertField :: (C.MonadThrow m) => P.TyField -> EnvStateT m TypeWithName
    insertField (P.Field var ty) = do
      ty <- getType ty
      insertVar var ty
      return ty
transExpr (P.FunctionCall sym exprs) = do
  callee <- getVar sym
  case callee of
    VarEntry _ -> C.throwM $ TypeException $ "'" -+- (S.name sym) -+- "' is not a function"
    FunEntry fields ret -> do
      args <- mapM transExpr exprs
      throwIf ((length args) /= (length fields)) (TypeException "argument count mismatch")
      flip mapM (zip fields args) $ \(fty, aty) -> do
        expectsType aty fty
      return $ ExprTy ret
transExpr (P.IntLit _) = returnTy Int
transExpr (P.StringLit _) = returnTy String
transExpr (P.UnitLit) = returnTy Unit
transExpr (P.BinaryExpr left op right) = do
  left  <- transExpr left
  right <- transExpr right

  if op == "+" || op == "-" || op == "*" || op == "/" || op == "&" || op == "|" then do
    -- Arithmetic and Boolean
    expectsType left $ anon Int
    expectsType right $ anon Int
    returnTy Int
  else do
    -- Comparison
    eqLeftAndRight <- eqType (ty_ left) (ty_ right)
    throwIf (not eqLeftAndRight) (TypeException "types of left and right of binary expression must equal")
    let ty = typeOf left
    if ty /= Int && ty /= String && op /= "=" && op /= "<>" then
      C.throwM $ TypeException $ "this type " -+- (quoteTy ty) -+- " cannot use the operator " -+- op
    else
      returnTy Int
transExpr (P.UnaryExpr _ e) = do
  e <- transExpr e
  expectsType e $ anon Int
  returnTy Int
transExpr (P.RecordCreation id pairs) = do
  ty' <- getType id
  case ty ty' of
    Record tyPairs -> do
      throwIf ((length pairs) /= (length tyPairs)) (TypeException $ "counts of fields are mismatched")

      flip mapM (zip pairs tyPairs) $ \((id, val), (tyId, tyTy)) -> do
        throwIf (id /= tyId) (TypeException "ids are mismatched")
        valTy <- transExpr val
        expectsType valTy tyTy
      return $ ExprTy ty'

    _ -> C.throwM $ TypeException $ "'" -+- (S.name id) -+- "' must be a record"
transExpr (P.AssignExpr lv expr) = do
  lv <- transLValue lv
  expr <- transExpr expr
  expectsType expr (ty_ lv)
  returnTy Unit

