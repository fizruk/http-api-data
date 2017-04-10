{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Web.Internal.HttpApiDataSpec (spec) where

import Control.Applicative
import Data.Int
import Data.Word
import Data.Time
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.ByteString as BS
import Data.ByteString.Builder (toLazyByteString)
import Data.Version
import qualified Data.UUID as UUID

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

encodedUrlPieceProp :: ToHttpApiData a => a -> Bool
encodedUrlPieceProp x = toLazyByteString (toEncodedUrlPiece (toUrlPiece x)) == toLazyByteString (toEncodedUrlPiece x)


checkUrlPiece :: forall a. (Eq a, ToHttpApiData a, FromHttpApiData a, Show a, Arbitrary a) => Proxy a -> String -> Spec
checkUrlPiece _ name = prop name (toUrlPiece <=> parseUrlPiece :: a -> Bool)

-- | Check with given generator
checkUrlPiece' :: forall a. (Eq a, ToHttpApiData a, FromHttpApiData a, Show a) => Gen a -> String -> Spec
checkUrlPiece' gen name = prop name $ forAll gen (toUrlPiece <=> parseUrlPiece)

-- | Check case insensitivity for @parseUrlPiece@.
checkUrlPieceI :: forall a. (Eq a, ToHttpApiData a, FromHttpApiData a, Arbitrary a) => Proxy a -> String -> Spec
checkUrlPieceI _ = checkUrlPiece (Proxy :: Proxy (RandomCase a))

-- | Check that 'toEncodedUrlPiece' is equivallent to default implementation.
checkEncodedUrlPiece :: forall a. (Show a, ToHttpApiData a, Arbitrary a) => Proxy a -> String -> Spec
checkEncodedUrlPiece _ = checkEncodedUrlPiece' (arbitrary :: Gen a)

-- | Check that 'toEncodedUrlPiece' is equivallent to default implementation.
-- Use a given generator.
checkEncodedUrlPiece' :: forall a. (Show a, ToHttpApiData a) => Gen a -> String -> Spec
checkEncodedUrlPiece' gen name = prop name $ forAll gen encodedUrlPieceProp

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
    checkUrlPiece' localTimeGen               "LocalTime"
    checkUrlPiece' zonedTimeGen               "ZonedTime"
    checkUrlPiece' utcTimeGen                 "UTCTime"
    checkUrlPiece' nominalDiffTimeGen         "NominalDiffTime"
    checkUrlPiece  (Proxy :: Proxy Version)   "Version"
    checkUrlPiece' uuidGen                    "UUID"

    checkUrlPiece  (Proxy :: Proxy (Maybe String))            "Maybe String"
    checkUrlPieceI (Proxy :: Proxy (Maybe Integer))           "Maybe Integer"
    checkUrlPiece  (Proxy :: Proxy (Either Integer T.Text))   "Either Integer Text"
    checkUrlPieceI (Proxy :: Proxy (Either Version Day))      "Either Version Day"

#if MIN_VERSION_base(4,8,0)
    checkUrlPiece  (Proxy :: Proxy Natural)   "Natural"
#endif

  describe "toEncodedUrlPiece encodes correctly" $ do
    checkEncodedUrlPiece  (Proxy :: Proxy ())        "()"
    checkEncodedUrlPiece  (Proxy :: Proxy Char)      "Char"
    checkEncodedUrlPiece  (Proxy :: Proxy Bool)      "Bool"
    checkEncodedUrlPiece  (Proxy :: Proxy Ordering)  "Ordering"
    checkEncodedUrlPiece  (Proxy :: Proxy Int)       "Int"
    checkEncodedUrlPiece  (Proxy :: Proxy Int8)      "Int8"
    checkEncodedUrlPiece  (Proxy :: Proxy Int16)     "Int16"
    checkEncodedUrlPiece  (Proxy :: Proxy Int32)     "Int32"
    checkEncodedUrlPiece  (Proxy :: Proxy Int64)     "Int64"
    checkEncodedUrlPiece  (Proxy :: Proxy Integer)   "Integer"
    checkEncodedUrlPiece  (Proxy :: Proxy Word)      "Word"
    checkEncodedUrlPiece  (Proxy :: Proxy Word8)     "Word8"
    checkEncodedUrlPiece  (Proxy :: Proxy Word16)    "Word16"
    checkEncodedUrlPiece  (Proxy :: Proxy Word32)    "Word32"
    checkEncodedUrlPiece  (Proxy :: Proxy Word64)    "Word64"
    checkEncodedUrlPiece  (Proxy :: Proxy String)    "String"
    checkEncodedUrlPiece  (Proxy :: Proxy T.Text)    "Text.Strict"
    checkEncodedUrlPiece  (Proxy :: Proxy L.Text)    "Text.Lazy"
    checkEncodedUrlPiece  (Proxy :: Proxy Day)       "Day"
    checkEncodedUrlPiece' localTimeGen               "LocalTime"
    checkEncodedUrlPiece' zonedTimeGen               "ZonedTime"
    checkEncodedUrlPiece' utcTimeGen                 "UTCTime"
    checkEncodedUrlPiece' nominalDiffTimeGen         "NominalDiffTime"
    checkEncodedUrlPiece  (Proxy :: Proxy Version)   "Version"
    checkEncodedUrlPiece' uuidGen                    "UUID"

    checkEncodedUrlPiece  (Proxy :: Proxy (Maybe String))            "Maybe String"
    checkEncodedUrlPiece  (Proxy :: Proxy (Maybe Integer))           "Maybe Integer"
    checkEncodedUrlPiece  (Proxy :: Proxy (Either Integer T.Text))   "Either Integer Text"
    checkEncodedUrlPiece  (Proxy :: Proxy (Either Version Day))      "Either Version Day"

#if MIN_VERSION_base(4,8,0)
    checkEncodedUrlPiece  (Proxy :: Proxy Natural)   "Natural"
#endif

  it "bad integers are rejected" $ do
    parseUrlPieceMaybe (T.pack "123hello") `shouldBe` (Nothing :: Maybe Int)

  it "bounds checking works" $ do
    parseUrlPieceMaybe (T.pack "256") `shouldBe` (Nothing :: Maybe Int8)
    parseUrlPieceMaybe (T.pack "-10") `shouldBe` (Nothing :: Maybe Word)

  it "invalid utf8 is handled" $ do
    parseHeaderMaybe (BS.pack [128]) `shouldBe` (Nothing :: Maybe T.Text)


uuidGen :: Gen UUID.UUID
uuidGen = UUID.fromWords <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- TODO: this generators don't generate full range items
localTimeGen :: Gen LocalTime
localTimeGen = LocalTime
    <$> arbitrary
    <*> liftA3 TimeOfDay (choose (0, 23)) (choose (0, 59)) (fromInteger <$> choose (0, 60))

zonedTimeGen :: Gen ZonedTime
zonedTimeGen = ZonedTime
    <$> localTimeGen -- Note: not arbitrary!
    <*> liftA3 TimeZone arbitrary arbitrary (vectorOf 3 (elements ['A'..'Z']))

utcTimeGen :: Gen UTCTime
utcTimeGen = UTCTime <$> arbitrary <*> fmap fromInteger (choose (0, 86400))

nominalDiffTimeGen :: Gen NominalDiffTime
nominalDiffTimeGen = fromInteger <$> arbitrary
