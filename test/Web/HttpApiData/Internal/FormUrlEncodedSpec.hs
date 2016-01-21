{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Web.HttpApiData.Internal.FormUrlEncodedSpec (spec) where

import Test.Hspec
import qualified Data.Map as M
import Test.QuickCheck

import Web.HttpApiData.Internal.FormUrlEncoded
import Web.HttpApiData.Internal.TestInstances

spec :: Spec
spec = do
  genericSpec

genericSpec :: Spec
genericSpec = describe "Default (generic) instances" $ do

  context "ToFormUrlEncoded" $ do

    it "contains the record names" $ property $ \(x :: SimpleRec) -> do
      let f = unForm $ toForm x
      M.member "rec1" f `shouldBe` True
      M.member "rec2" f `shouldBe` True

    it "contains the correct record values" $ property $ \(x :: SimpleRec) -> do
      let f = unForm $ toForm x
      M.lookup "rec1" f `shouldBe` Just (Just (rec1 x))
      M.lookup "rec2" f `shouldBe` Just (Just (rec2 x))
