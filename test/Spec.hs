{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Test.Hspec
import Control.Exception (evaluate)
import Control.Monad.State
import Data.String.Here
import qualified Data.IntMap.Strict as IM

import qualified Tiger.Parser as P
import qualified Tiger.Types as Ty

parseAndTypecheck src =
  case P.parse "" src of
    Left err -> Nothing
    Right (ast, symbols) -> do
      evalStateT (Ty.transExpr ast) (IM.empty, Ty.defaultTEnv symbols) :: Maybe Ty.ExprTy

shouldBeType a ty = a `shouldBe` (Just (Ty.ExprTy ty))

main :: IO ()
main = hspec $ do
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

