{-# LANGUAGE OverloadedStrings #-}
module Web.HttpApiData.Internal.FormUrlEncodedSpec (spec) where

import Test.Hspec
import qualified Data.HashMap as H

import Web.HttpApiData.Internal.FormUrlEncoded

spec :: Spec
spec = do
  genericSpec

genericSpec :: Spec
genericSpec = describe "Default (generic) instances" $ do

  context "ToFormUrlEncoded" $ do

    it "contains the record names" $ property $ \x ->
      H.member "name" (unForm $ toForm x) `shouldBe` True
      H.member "age" (unForm $ toForm x) `shouldBe` True

    it "contains the correct record values" $ property $ \x ->
      H.lookup "name" (unForm $ toForm x) `shouldBe` Just (name x)
      H.lookup "age" (unForm $ toForm x) `shouldBe` Just (age x)
