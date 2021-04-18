{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Test.Hspec
import Control.Exception (evaluate)
import Control.Monad.State
import Data.String.Here
import Data.Either
import System.Directory
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as IO
import qualified Data.IntMap.Strict as IM

import qualified Tiger.Parser as P
import qualified Tiger.Types as Ty

parse = P.parse ""

parseAndTypecheck src =
  case P.parse "" src of
    Left err -> Nothing
    Right (ast, symbols) -> do
      evalStateT (Ty.transExpr ast) (IM.empty, Ty.defaultTEnv symbols) :: Maybe Ty.ExprTy

shouldBeType a ty = a `shouldBe` (Just (Ty.ExprTy ty))

shouldBeRight a = shouldSatisfy a isRight

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
      it (file ++ " should be parsed") $ do
        s <- IO.readFile $ "./testcases/" ++ file
        parse s `shouldSatisfy` isRight
    -- flip mapM files $ \file -> do
    --   it (file ++ " can be compiled") $ do
    --     s <- IO.readFile $ "./testcases/" ++ file
    --     if T.isInfixOf "error" s then
    --       parseAndTypecheck s `shouldNotSatisfy` isJust
    --     else
    --       parseAndTypecheck s `shouldSatisfy` isJust
    return ()

