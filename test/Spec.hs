{-# Language ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Control.Applicative

import Data.Int
import Data.Char
import Data.Word
import Data.Time
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Data.Version

import Test.Hspec
import Test.Hspec.QuickCheck(prop)
import Test.QuickCheck

import Web.HttpApiData
import Web.HttpApiData.Internal

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary L.Text where
  arbitrary = L.pack <$> arbitrary

instance Arbitrary Day where
  arbitrary = liftA3 fromGregorian (fmap abs arbitrary) arbitrary arbitrary

instance Arbitrary Version where
  arbitrary = (version . map abs) <$> nonempty
    where
      version branch = Version branch []
      nonempty = liftA2 (:) arbitrary arbitrary

main :: IO ()
main = hspec spec

(<=>) :: Eq a => (a -> b) -> (b -> Either T.Text a) -> a -> Bool
(f <=> g) x = g (f x) == Right x

data Proxy a = Proxy

checkUrlPiece :: forall a. (Eq a, ToHttpApiData a, FromHttpApiData a, Show a, Arbitrary a) => Proxy a -> String -> Spec
checkUrlPiece _ name = prop name (toUrlPiece <=> parseUrlPiece :: a -> Bool)

data RandomCase a = RandomCase [Bool] a

instance ToHttpApiData a => Show (RandomCase a) where
  show rc@(RandomCase _ x) = show (toUrlPiece rc) ++ " (original: " ++ show (toUrlPiece x) ++ ")"

instance Eq a => Eq (RandomCase a) where
  RandomCase _ x == RandomCase _ y = x == y

instance Arbitrary a => Arbitrary (RandomCase a) where
  arbitrary = liftA2 RandomCase nonempty arbitrary
    where
      nonempty = liftA2 (:) arbitrary arbitrary

instance ToHttpApiData a => ToHttpApiData (RandomCase a) where
  toUrlPiece (RandomCase us x) = T.pack (zipWith (\u -> if u then toUpper else toLower) (cycle us) (T.unpack (toUrlPiece x)))

instance FromHttpApiData a => FromHttpApiData (RandomCase a) where
  parseUrlPiece s = RandomCase [] <$> parseUrlPiece s

-- | Check case insensitivity for @parseUrlPiece@.
checkUrlPieceI :: forall a. (Eq a, ToHttpApiData a, FromHttpApiData a, Show a, Arbitrary a) => Proxy a -> String -> Spec
checkUrlPieceI _ = checkUrlPiece (Proxy :: Proxy (RandomCase a))

spec :: Spec
spec = do
  describe "toUrlPiece <=> parseUrlPiece" $ do
    checkUrlPiece  (Proxy :: Proxy ())        "()"
    checkUrlPiece  (Proxy :: Proxy Char)      "Char"
    checkUrlPieceI (Proxy :: Proxy Bool)      "Bool"
    checkUrlPieceI (Proxy :: Proxy Ordering)  "Ordering"
    checkUrlPiece  (Proxy :: Proxy Int)       "Int"
    checkUrlPiece  (Proxy :: Proxy Int8)      "Int8"
    checkUrlPiece  (Proxy :: Proxy Int16)     "Int16"
    checkUrlPiece  (Proxy :: Proxy Int32)     "Int32"
    checkUrlPiece  (Proxy :: Proxy Int64)     "Int64"
    checkUrlPiece  (Proxy :: Proxy Integer)   "Integer"
    checkUrlPiece  (Proxy :: Proxy Word)      "Word"
    checkUrlPiece  (Proxy :: Proxy Word8)     "Word8"
    checkUrlPiece  (Proxy :: Proxy Word16)    "Word16"
    checkUrlPiece  (Proxy :: Proxy Word32)    "Word32"
    checkUrlPiece  (Proxy :: Proxy Word64)    "Word64"
    checkUrlPiece  (Proxy :: Proxy String)    "String"
    checkUrlPiece  (Proxy :: Proxy T.Text)    "Text.Strict"
    checkUrlPiece  (Proxy :: Proxy L.Text)    "Text.Lazy"
    checkUrlPiece  (Proxy :: Proxy Day)       "Day"
    checkUrlPiece  (Proxy :: Proxy Version)   "Version"

    checkUrlPiece  (Proxy :: Proxy (Maybe String))            "Maybe String"
    checkUrlPieceI (Proxy :: Proxy (Maybe Integer))           "Maybe Integer"
    checkUrlPiece  (Proxy :: Proxy (Either Integer T.Text))   "Either Integer Text"
    checkUrlPieceI (Proxy :: Proxy (Either Version Day))      "Either Version Day"

  it "bad integers are rejected" $ do
    parseUrlPieceMaybe (T.pack "123hello") `shouldBe` (Nothing :: Maybe Int)

  it "bounds checking works" $ do
    parseUrlPieceMaybe (T.pack "256") `shouldBe` (Nothing :: Maybe Int8)
    parseUrlPieceMaybe (T.pack "-10") `shouldBe` (Nothing :: Maybe Word)

