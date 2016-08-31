{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Web.Internal.HttpApiDataSpec (spec) where

import Data.Int
import Data.Word
import Data.Time
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.ByteString as BS
import Data.Version

import Data.Proxy

#if MIN_VERSION_base(4,8,0)
import Numeric.Natural
#endif

import Test.Hspec
import Test.Hspec.QuickCheck(prop)
import Test.QuickCheck

import Web.Internal.HttpApiData

import Web.Internal.TestInstances

(<=>) :: Eq a => (a -> b) -> (b -> Either T.Text a) -> a -> Bool
(f <=> g) x = g (f x) == Right x

checkUrlPiece :: forall a. (Eq a, ToHttpApiData a, FromHttpApiData a, Show a, Arbitrary a) => Proxy a -> String -> Spec
checkUrlPiece _ name = prop name (toUrlPiece <=> parseUrlPiece :: a -> Bool)

-- | Check case insensitivity for @parseUrlPiece@.
checkUrlPieceI :: forall a. (Eq a, ToHttpApiData a, FromHttpApiData a, Arbitrary a) => Proxy a -> String -> Spec
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
    checkUrlPiece  (Proxy :: Proxy LocalTime) "LocalTime"
    checkUrlPiece  (Proxy :: Proxy ZonedTime) "ZonedTime"
    checkUrlPiece  (Proxy :: Proxy UTCTime)   "UTCTime"
    checkUrlPiece  (Proxy :: Proxy NominalDiffTime) "NominalDiffTime"
    checkUrlPiece  (Proxy :: Proxy Version)   "Version"

    checkUrlPiece  (Proxy :: Proxy (Maybe String))            "Maybe String"
    checkUrlPieceI (Proxy :: Proxy (Maybe Integer))           "Maybe Integer"
    checkUrlPiece  (Proxy :: Proxy (Either Integer T.Text))   "Either Integer Text"
    checkUrlPieceI (Proxy :: Proxy (Either Version Day))      "Either Version Day"

#if MIN_VERSION_base(4,8,0)
    checkUrlPiece  (Proxy :: Proxy Natural)   "Natural"
#endif

  it "bad integers are rejected" $ do
    parseUrlPieceMaybe (T.pack "123hello") `shouldBe` (Nothing :: Maybe Int)

  it "bounds checking works" $ do
    parseUrlPieceMaybe (T.pack "256") `shouldBe` (Nothing :: Maybe Int8)
    parseUrlPieceMaybe (T.pack "-10") `shouldBe` (Nothing :: Maybe Word)

  it "invalid utf8 is handled" $ do
    parseHeaderMaybe (BS.pack [128]) `shouldBe` (Nothing :: Maybe T.Text)
