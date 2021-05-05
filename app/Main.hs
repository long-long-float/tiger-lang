{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import Data.Int (Int)
import Control.Monad.State
import System.Environment (getArgs)
import Text.Megaparsec hiding (State)
import qualified Control.Monad.Catch as C
import qualified Data.Text as T
import qualified Data.Text.IO as IO

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
      print symbols
      case evalStateT (Ty.transExpr ast) (Ty.defaultEnv symbols) :: Either C.SomeException Ty.ExprTy of
        Right typed -> print typed
        Left ex -> case C.fromException ex of
          Just (Ty.TypeException msg) -> IO.putStrLn msg
          Nothing -> print ex

