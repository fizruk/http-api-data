{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Web.Internal.FormUrlEncodedSpec (spec) where

import Control.Monad ((<=<))
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Map.Strict            as Map
import Data.Text (Text, unpack)
import Test.Hspec
import Test.QuickCheck

import GHC.Exts (fromList)

import Web.Internal.FormUrlEncoded
import Web.Internal.HttpApiData
import Web.Internal.TestInstances

spec :: Spec
spec = do
  genericSpec
  urlEncoding

genericSpec :: Spec
genericSpec = describe "Default (generic) instances" $ do

  context "ToForm" $ do

    it "contains the record names" $ property $ \(x :: SimpleRec) -> do
      let f = unForm $ toForm x
      Map.member "rec1" f `shouldBe` True
      Map.member "rec2" f `shouldBe` True

    it "contains the correct record values" $ property $ \(x :: SimpleRec) -> do
      let f = unForm $ toForm x
      Map.lookup "rec1" f `shouldBe` Just [rec1 x]
      (parseQueryParams <$> Map.lookup "rec2" f) `shouldBe` Just (Right [rec2 x])

  context "FromForm" $ do

    it "is the right inverse of ToForm" $ property $ \x (y :: Int) -> do
      let f1 = fromList [("rec1", x), ("rec2", toQueryParam y)]
          Right r1 = fromForm f1 :: Either Text SimpleRec
      toForm r1 `shouldBe` f1

    it "returns the underlying error" $ do
      let f = fromList [("rec1", "anything"), ("rec2", "bad")]
          Left e = fromForm f :: Either Text SimpleRec
      unpack e `shouldContain` "input does not start with a digit"

urlEncoding :: Spec
urlEncoding = describe "urlEncoding" $ do

  it "urlDecodeForm (urlEncodeForm x) == Right x" $ property $ \(NoEmptyKeyForm x) -> do
    urlDecodeForm (urlEncodeForm x) `shouldBe` Right x

  it "urlDecodeAsForm == (fromForm <=< urlDecodeForm)" $ property $ \(x :: BSL.ByteString) -> do
    (urlDecodeAsForm x :: Either Text Form) `shouldBe` (fromForm <=< urlDecodeForm) x

  it "urlEncodeAsForm == urlEncodeForm . toForm" $ property $ \(x :: Form) -> do
    urlEncodeAsForm x `shouldBe` (urlEncodeForm . toForm) x

  it "urlDecodeForm \"\" == Right mempty" $ do
    urlDecodeForm "" `shouldBe` Right mempty
