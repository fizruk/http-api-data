{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Web.Internal.FormUrlEncodedSpec (spec) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
import Data.Monoid
#endif

import Control.Monad ((<=<))
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.HashMap.Strict as HashMap
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
      HashMap.member "rec1" f `shouldBe` True
      HashMap.member "rec2" f `shouldBe` True

    it "contains the correct record values" $ property $ \(x :: SimpleRec) -> do
      let f = unForm $ toForm x
      HashMap.lookup "rec1" f `shouldBe` Just [rec1 x]
      (parseQueryParams <$> HashMap.lookup "rec2" f) `shouldBe` Just (Right [rec2 x])

    context "for sum types" $ do

      it "contains the correct records" $ property $ \x  -> do
        let f = unForm $ toForm x
        case x of
          SSRLeft _ _ -> do
            parseQueryParams <$> HashMap.lookup "left1" f `shouldBe` Just (Right [left1 x])
            parseQueryParams <$> HashMap.lookup "left2" f `shouldBe` Just (Right [left2 x])
          SSRRight _ _ -> do
            parseQueryParams <$> HashMap.lookup "right1" f `shouldBe` Just (Right [right1 x])
            parseQueryParams <$> HashMap.lookup "right2" f `shouldBe` Just (Right [right2 x])


  context "FromForm" $ do

    it "is the left inverse of ToForm" $ property $
      \(x :: SimpleRec, y :: SimpleSumRec) -> do
        fromForm (toForm x) `shouldBe` Right x
        fromForm (toForm y) `shouldBe` Right y

    it "is the right inverse of ToForm" $ property $ \x (y :: Int) -> do
      let f1 = fromList [("rec1", x), ("rec2", toQueryParam y)]
          Right r1 = fromForm f1 :: Either Text SimpleRec
      toForm r1 `shouldBe` f1
      let f2 = fromList [("right1", x), ("right2", toQueryParam y)]
          Right r2 = fromForm f2 :: Either Text SimpleSumRec
      toForm r2 `shouldBe` f2

    it "returns the underlying error" $ do
      let f = fromList [("rec1", "anything"), ("rec2", "bad")]
          Left e = fromForm f :: Either Text SimpleRec
      unpack e `shouldContain` "input does not start with a digit"

urlEncoding :: Spec
urlEncoding = describe "urlEncoding" $ do

  it "decodeForm (encodeForm x) == Right x" $ property $ \(NoEmptyKeyForm x) -> do
    decodeForm (encodeForm x) `shouldBe` Right x

  it "decodeAsForm == (fromForm <=< decodeForm)" $ property $ \(x :: BSL.ByteString) -> do
    (decodeAsForm x :: Either Text Form) `shouldBe` (fromForm <=< decodeForm) x

  it "encodeAsForm == encodeForm . toForm" $ property $ \(x :: Form) -> do
    encodeAsForm x `shouldBe` (encodeForm . toForm) x

  it "decodeForm \"\" == Right mempty" $ do
    decodeForm "" `shouldBe` Right mempty
