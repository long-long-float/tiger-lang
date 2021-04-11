{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import Data.Int (Int)
import Data.Maybe
import Control.Monad.State
import System.Environment (getArgs)
import Text.Megaparsec hiding (State)
import qualified Control.Monad.Catch as C
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import qualified Data.Text.IO as IO
import qualified Safe as S
import qualified Data.Map.Strict as M

import Tiger.Symbol
import qualified Tiger.Parser as P
import qualified Tiger.Types as Ty

main :: IO ()
main = do
  args <- getArgs
  let srcPath = args !! 0
  s <- IO.readFile srcPath
  case P.parse srcPath s of
    Left err -> putStr $ errorBundlePretty err
    Right (ast, symbols) -> do
      print ast

      let typed = evalStateT (Ty.transExpr ast) (IM.empty, Ty.defaultTEnv symbols) :: Either C.SomeException Ty.ExprTy
      print typed

