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
  -- | Record [(Symbol, Type)]
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
fromAbstType (P.RecordTy fields) = C.throwM $ UnimplementedException "fromAbstType"
fromAbstType (P.ArrayTy inner) = do
  inner <- getType inner
  return $ Array inner

(-+-) :: Text -> Text -> Text
a -+- b = T.append a b

(-+$) :: Text -> String -> Text
a -+$ b = T.append a (T.pack b)

quoteTy :: Type -> Text
quoteTy ty = "'" -+$ (show ty) -+- "'"

expectsType :: (C.MonadThrow m) => ExprTy -> Type -> m ()
expectsType ty expected = do
  if typ ty /= expected then
    C.throwM $ TypeException $ "Couldn't match expected type " -+- (quoteTy $ typ ty) -+- " with actual type " -+- (quoteTy expected)
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
transExpr (P.ArrayCreation inner n v) = do
  inner <- getType inner
  n <- transExpr n
  v <- transExpr v
  expectsType n Int
  expectsType v inner
  return $ ExprTy $ Array inner
transExpr (P.IfExpr cond t f) = do
  cond <- transExpr cond
  t <- transExpr t
  f <- transExpr f
  expectsType cond Int
  expectsType t (typ f)
  return t
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
                Nothing -> return $ typ init
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
      let ve' = IM.insert (id sym) (FunEntry fieldTypes ret) ve
      put (ve', te)

    insertField :: (C.MonadThrow m) => P.TyField -> EnvStateT m Type
    insertField (P.Field var ty) = do
      (ve, te) <- get
      ty <- getType ty
      let ve' = IM.insert (id var) (VarEntry ty) ve
      put (ve', te)
      return ty
transExpr (P.FunctionCall sym exprs) = do
  callee <- getVar sym
  case callee of
    VarEntry _ -> C.throwM $ TypeException $ "'" -+- (name sym) -+- "' is not a function"
    FunEntry fields ret -> do
      args <- mapM transExpr exprs
      if (length args) /= (length fields) then
        C.throwM $ TypeException "argument count mismatch"
      else
        return $ ExprTy ret
transExpr (P.IntLit _) = return $ ExprTy Int
transExpr (P.StringLit _) = return $ ExprTy String
transExpr (P.BinaryExpr left _ right) = do
  left <- transExpr left
  right <- transExpr right
  expectsType left Int
  expectsType right Int
  return $ ExprTy Int
transExpr (P.UnaryExpr _ e) = do
  e <- transExpr e
  expectsType e Int
  return $ ExprTy Int

