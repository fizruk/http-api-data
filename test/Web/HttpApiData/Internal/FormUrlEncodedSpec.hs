{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Web.HttpApiData.Internal.FormUrlEncodedSpec (spec) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Test.Hspec
import qualified Data.Map as M
import Test.QuickCheck

import Web.HttpApiData.Internal.FormUrlEncoded
import Web.HttpApiData.Internal.HttpApiData
import Web.HttpApiData.Internal.TestInstances

spec :: Spec
spec = do
  genericSpec
  urlEncoding

genericSpec :: Spec
genericSpec = describe "Default (generic) instances" $ do

  context "ToForm" $ do

    it "contains the record names" $ property $ \(x :: SimpleRec) -> do
      let f = unForm $ toForm x
      M.member "rec1" f `shouldBe` True
      M.member "rec2" f `shouldBe` True

    it "contains the correct record values" $ property $ \(x :: SimpleRec) -> do
      let f = unForm $ toForm x
      M.lookup "rec1" f `shouldBe` Just (rec1 x)
      (parseUrlPiece <$> M.lookup "rec2" f) `shouldBe` Just (Right $ rec2 x)

    context "for sum types" $ do

      it "contains the correct records" $ property $ \x  -> do
        let f = unForm $ toForm x
        case x of
          SSRLeft _ _ -> do
            parseUrlPiece <$> M.lookup "left1" f `shouldBe` Just (Right $ left1 x)
            parseUrlPiece <$> M.lookup "left2" f `shouldBe` Just (Right $ left2 x)
          SSRRight _ _ -> do
            parseUrlPiece <$> M.lookup "right1" f `shouldBe` Just (Right $ right1 x)
            parseUrlPiece <$> M.lookup "right2" f `shouldBe` Just (Right $ right2 x)


  context "FromForm" $ do

    it "is the left inverse of ToForm" $ property $
      \(x :: SimpleRec, y :: SimpleSumRec) -> do
        fromForm (toForm x) `shouldBe` Right x
        fromForm (toForm y) `shouldBe` Right y

    it "is the right inverse of ToForm" $ property $ \x (y :: Int) -> do
      let f1 = Form $ M.fromList [("rec1", x), ("rec2", toUrlPiece y)]
          Right r1 = fromForm f1 :: Either String SimpleRec
      toForm r1 `shouldBe` f1
      let f2 = Form $ M.fromList [("right1", x), ("right2", toUrlPiece y)]
          Right r2 = fromForm f2 :: Either String SimpleSumRec
      toForm r2 `shouldBe` f2

    it "returns the underlying error" $ do
      let f = Form $ M.fromList [("rec1", "anything"), ("rec2", "bad")]
          Left e = fromForm f :: Either String SimpleRec
      e `shouldContain` "input does not start with a digit"

urlEncoding :: Spec
urlEncoding = describe "urlEncoding" $ do

  it "decodeForm (encodeForm x) == Right x" $ property $ \(NoEmptyKeyForm x) -> do
    decodeForm (encodeForm x) `shouldBe` Right x
