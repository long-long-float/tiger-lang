{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Test.Hspec
import Control.Exception (evaluate)
import Control.Monad.State
import Data.String.Here
import Data.Either
import Data.List
import Data.Maybe
import System.Directory
import qualified Data.Text as T
import qualified Data.Text.IO as IO
import qualified Data.IntMap.Strict as IM

import qualified Tiger.Parser as P
import qualified Tiger.Types as Ty
import qualified Tiger.Symbol as S

parse = P.parse ""

parseAndTypecheck src =
  case P.parse "" src of
    Left err -> Nothing
    Right (ast, symbols) -> do
      evalStateT (Ty.transExpr ast) (Ty.defaultEnv symbols) :: Maybe Ty.ExprTy

shouldBeType a ty = a `shouldBe` (Just (Ty.ExprTy $ Ty.TypeWithName ty S.emptySymbol))

-- From testcases
haveParseError = ["test49.tig"]
haveTypeError = ["test43.tig"]

main :: IO ()
main = hspec $ do
  describe "Parser" $ do
    it "can be parsed a string with escape" $ do
      parse "\"Hello\\n\"" `shouldSatisfy` isRight
      parse "\"Quote \\\"\"" `shouldSatisfy` isRight
  describe "Type checker" $ do
    it "1 should be Int" $ do
      parseAndTypecheck "1" `shouldBeType` Ty.Int
    it "arithmetic expression should be Int" $ do
      parseAndTypecheck "1 + 2" `shouldBeType` Ty.Int
      parseAndTypecheck "1 * 2" `shouldBeType` Ty.Int
      parseAndTypecheck "(1 + 2) * (3 - 4) / (5 * 6)" `shouldBeType` Ty.Int
      parseAndTypecheck [here|
1 + 2
|] `shouldBeType` Ty.Int

    it "\"Test\" should be String" $ do
      parseAndTypecheck "\"Test\"" `shouldBeType` Ty.String

  describe "From testcases" $ do
    files <- runIO $ listDirectory "./testcases"

    flip mapM files $ \file -> do
      s <- runIO $ IO.readFile $ "./testcases/" ++ file

      let err = any (\f -> isInfixOf f file) haveParseError
      if not err then
        it (file ++ " should be parsed") $ do
          parse s `shouldSatisfy` isRight
      else
        it (file ++ " should raise a parse error") $ do
          parse s `shouldNotSatisfy` isRight

    flip mapM files $ \file -> do
      s <- runIO $ IO.readFile $ "./testcases/" ++ file

      let err = any (\f -> isInfixOf f file) haveTypeError
      if err || T.isInfixOf "error" s then
        it (file ++ " should have type error(s)") $ do
        parseAndTypecheck s `shouldNotSatisfy` isJust
      else
        it (file ++ " should be compiled") $ do
        parseAndTypecheck s `shouldSatisfy` isJust

    return ()

