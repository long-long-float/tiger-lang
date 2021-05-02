{-# LANGUAGE OverloadedStrings #-}

module Tiger.Types where

import Prelude hiding (id)
import Data.Text (Text)
import Data.Maybe
import Control.Monad.State
import qualified Control.Monad.Catch as C
import qualified Data.IntMap.Strict as IM
import qualified Safe as S
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Tiger.Symbol
import qualified Tiger.Parser as P

data Type
  = Int
  | String
  | Record [(Symbol, Type)]
  | Array Type -- TODO: Add unique
  | Nil
  | Unit
  -- | Name
  deriving (Eq, Ord, Show)

type Table = IM.IntMap

data EnvEntry
  = VarEntry Type
  | FunEntry { formals :: [Type], result :: Type }
  deriving (Eq, Ord, Show)

type VEnv = Table EnvEntry
type TEnv = Table Type
type EnvStateT m a = StateT (VEnv, TEnv) m a

data ExprTy = ExprTy { typ :: Type }
  deriving (Eq, Ord, Show)

data TypeException = TypeException Text deriving (Show)
instance C.Exception TypeException
data UnimplementedException = UnimplementedException Text deriving (Show)
instance C.Exception UnimplementedException

getSymbolId :: Text -> SymbolTable -> Int
getSymbolId name st =
  fromMaybe 0 (M.lookup name st)

defaultTEnv :: SymbolTable -> TEnv
defaultTEnv symbols = IM.fromList [(getSymbolId "int" symbols, Int), (getSymbolId "string" symbols, String)]

fromAbstType :: (C.MonadThrow m) => P.Type -> EnvStateT m Type
fromAbstType (P.NameTy sym) = getType sym
fromAbstType (P.RecordTy fields) = do
  pairs <- mapM (\(P.Field id ty) -> do
    ty <- getType ty
    return (id, ty)) fields
  return $ Record pairs
fromAbstType (P.ArrayTy inner) = do
  inner <- getType inner
  return $ Array inner

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

eqType :: Type -> Type -> Bool
eqType Nil (Record _) = True
eqType (Record _) Nil = True
eqType t1 t2 = t1 == t2

expectsType :: (C.MonadThrow m) => ExprTy -> Type -> m ()
expectsType ty expected = do
  if not $ eqType (typ ty) expected then
    C.throwM $ TypeException $ "Couldn't match expected type " -+- (quoteTy expected) -+- " with actual type " -+- (quoteTy $ typ ty)
  else
    return ()

getType :: (C.MonadThrow m) => Symbol -> EnvStateT m Type
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

insertVar :: (C.MonadThrow m) => Symbol -> Type -> EnvStateT m ()
insertVar var ty = do
  (ve, te) <- get
  let ve' = IM.insert (id var) (VarEntry ty) ve
  put (ve', te)
  return ()

transLValue :: (C.MonadThrow m) => P.LValue -> EnvStateT m ExprTy
transLValue (P.SimpleVar sym) = do
  (ve, te) <- get
  case IM.lookup (id sym) ve of
    Just (VarEntry var) -> return $ ExprTy var
    Nothing ->  C.throwM $ TypeException $ "Undefined variable: " -+- (name sym)
transLValue _ = C.throwM $ TypeException "undef@lvalue"

transExpr :: (C.MonadThrow m) => P.Expr -> EnvStateT m ExprTy
transExpr (P.ValueExpr lv) = transLValue lv
transExpr P.NilExpr = return $ ExprTy Nil
transExpr (P.SeqExpr exprs) = do
  types <- mapM transExpr exprs
  return $ S.lastDef (ExprTy Unit) types
transExpr (P.ArrayCreation ty n v) = do
  ty <- getType ty
  case ty of
    Array inner -> do
      n <- transExpr n
      v <- transExpr v
      expectsType n Int
      expectsType v inner
      return $ ExprTy ty
    _ -> C.throwM $ TypeException $ (show ty) $+- " must be array"
transExpr (P.IfExpr cond t f) = do
  cond <- transExpr cond
  t <- transExpr t
  expectsType cond Int
  case f of
    Just f -> do
      f <- transExpr f
      expectsType t (typ f)
      return t
    Nothing -> do
      expectsType t Unit
      return $ ExprTy Unit
transExpr (P.WhileExpr cond body) = do
  cond <- transExpr cond
  body <- transExpr body
  expectsType cond Int
  expectsType body Unit
  return $ ExprTy Unit
transExpr (P.ForExpr id begin end body) = do
  begin <- transExpr begin
  end <- transExpr end
  body <- transExpr body
  expectsType begin Int
  expectsType end Int
  (ve, te) <- get
  insertVar id Int
  expectsType body Unit
  put (ve, te)
  return $ ExprTy Unit
transExpr (P.LetExpr decs exprs) = do
  env <- get
  mapM transDecs decs
  types <- mapM transExpr exprs
  put env
  return $ S.lastDef (ExprTy Unit) types
  where
    transDecs :: (C.MonadThrow m) => P.Declaration -> EnvStateT m ()
    transDecs (P.TypeDec sym ty) = do
      (ve, te) <- get
      ty <- fromAbstType ty
      let te' = IM.insert (id sym) ty te
      put (ve, te')
    transDecs (P.VarDec sym ty init) = do
      (ve, te) <- get
      init <- transExpr init
      ty' <- case ty of
                Just ty' -> getType ty'
                Nothing -> do
                  throwIf ((typ init) == Nil) (TypeException "initializing nil not constrained by record type is not accepted")
                  return $ typ init
      expectsType init ty'
      let ve' = IM.insert (id sym) (VarEntry ty') ve
      put (ve', te)
    transDecs (P.FunDec sym fields ty body) = do
      (ve, te) <- get
      fieldTypes <- mapM insertField fields
      actualRet <- transExpr body
      ret <- case ty of
              Just ret -> getType ret
              Nothing -> return $ typ actualRet
      expectsType actualRet ret

      put (ve, te)
      let ve' = IM.insert (id sym) (FunEntry fieldTypes ret) ve
      put (ve', te)

    insertField :: (C.MonadThrow m) => P.TyField -> EnvStateT m Type
    insertField (P.Field var ty) = do
      ty <- getType ty
      insertVar var ty
      return ty
transExpr (P.FunctionCall sym exprs) = do
  callee <- getVar sym
  case callee of
    VarEntry _ -> C.throwM $ TypeException $ "'" -+- (name sym) -+- "' is not a function"
    FunEntry fields ret -> do
      args <- mapM transExpr exprs
      throwIf ((length args) /= (length fields)) (TypeException "argument count mismatch")
      flip mapM (zip fields args) $ \(fty, aty) -> do
        expectsType aty fty
      return $ ExprTy ret
transExpr (P.IntLit _) = return $ ExprTy Int
transExpr (P.StringLit _) = return $ ExprTy String
transExpr (P.UnitLit) = return $ ExprTy Unit
transExpr (P.BinaryExpr left op right) = do
  left  <- transExpr left
  right <- transExpr right

  if op == "+" || op == "-" || op == "*" || op == "/" || op == "&" || op == "|" then do
    -- Arithmetic and Boolean
    expectsType left Int
    expectsType right Int
    return $ ExprTy Int
  else do
    -- Comparison
    throwIf (not $ eqType (typ left) (typ right)) (TypeException "types of left and right of binary expression must equal")
    let ty = typ left
    if ty /= Int && ty /= String && op /= "=" && op /= "<>" then
      C.throwM $ TypeException $ "this type " -+- (quoteTy ty) -+- " cannot use the operator " -+- op
    else
      return $ ExprTy Int
transExpr (P.UnaryExpr _ e) = do
  e <- transExpr e
  expectsType e Int
  return $ ExprTy Int
transExpr (P.RecordCreation id pairs) = do
  ty <- getType id
  case ty of
    Record tyPairs -> do
      throwIf ((length pairs) /= (length tyPairs)) (TypeException $ "counts of fields are mismatched")

      flip mapM (zip pairs tyPairs) $ \((id, val), (tyId, tyTy)) -> do
        throwIf (id /= tyId) (TypeException "ids are mismatched")
        valTy <- transExpr val
        expectsType valTy tyTy
      return $ ExprTy ty

    _ -> C.throwM $ TypeException $ "'" -+- (name id) -+- "' must be a record"
transExpr (P.AssignExpr lv expr) = do
  -- TODO: check
  return $ ExprTy Unit

