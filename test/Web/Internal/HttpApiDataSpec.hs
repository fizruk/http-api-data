{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Web.Internal.HttpApiDataSpec (spec) where

import Control.Applicative
import qualified Data.Fixed as F
import Data.Int
import Data.Char
import Data.Word
import Data.Time
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as L
import qualified Data.ByteString as BS
import Data.ByteString.Builder (toLazyByteString)
import Data.Version
import qualified Data.UUID.Types as UUID
import Web.Cookie (SetCookie, defaultSetCookie, setCookieName, setCookieValue)

import Data.Proxy

#if MIN_VERSION_time(1,9,1)
import Data.Time (nominalDiffTimeToSeconds, secondsToNominalDiffTime)
#endif

#if MIN_VERSION_base(4,8,0)
import Numeric.Natural
#endif

import Test.Hspec
import Test.Hspec.QuickCheck(prop)
import Test.QuickCheck

import Web.Internal.HttpApiData

import Web.Internal.TestInstances

(<=>) :: forall a b. (Show a, Show b, Eq a) => (a -> b) -> (b -> Either T.Text a) -> a -> Property
(f <=> g) x = counterexample
    (show lhs' ++ " : " ++ show lhs ++ " /= " ++ show rhs)
    (lhs == rhs)
  where
    lhs' = f x
    lhs = g lhs' :: Either T.Text a
    rhs = Right x :: Either T.Text a

encodedUrlPieceProp :: ToHttpApiData a => a -> Property
encodedUrlPieceProp x = toLazyByteString (toEncodedUrlPiece (toUrlPiece x)) === toLazyByteString (toEncodedUrlPiece x)

-- | Check 'ToHttpApiData' and 'FromHttpApiData' compatibility
checkUrlPiece :: forall a. (Eq a, ToHttpApiData a, FromHttpApiData a, Show a, Arbitrary a) => Proxy a -> String -> Spec
checkUrlPiece _ = checkUrlPiece' (arbitrary :: Gen a)

checkUrlPiece' :: forall a. (Eq a, ToHttpApiData a, FromHttpApiData a, Show a) => Gen a -> String -> Spec
checkUrlPiece' gen name = describe name $ do
    prop "toUrlPiece <=> parseUrlPiece" $ forAll gen (toUrlPiece <=> parseUrlPiece :: a -> Property)
    prop "toQueryParam <=> parseQueryParam" $ forAll gen (toQueryParam <=> parseQueryParam :: a -> Property)
    prop "toHeader <=> parseHeader" $ forAll gen (toHeader <=> parseHeader :: a -> Property)
    prop "toEncodedUrlPiece encodes correctly" $ forAll gen encodedUrlPieceProp

-- | Check case insensitivity for @parseUrlPiece@.
checkUrlPieceI :: forall a. (Eq a, ToHttpApiData a, FromHttpApiData a, Arbitrary a) => Proxy a -> String -> Spec
checkUrlPieceI _ = checkUrlPiece (Proxy :: Proxy (RandomCase a))

spec :: Spec
spec = do
  describe "Instances" $ do
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
    checkUrlPiece' timeOfDayGen               "TimeOfDay"
    checkUrlPiece' localTimeGen               "LocalTime"
    checkUrlPiece' zonedTimeGen               "ZonedTime"
    checkUrlPiece' utcTimeGen                 "UTCTime"
    checkUrlPiece' nominalDiffTimeGen         "NominalDiffTime"
    checkUrlPiece  (Proxy :: Proxy Version)   "Version"
    checkUrlPiece' uuidGen                    "UUID"
    checkUrlPiece' setCookieGen               "Cookie"

    checkUrlPiece  (Proxy :: Proxy F.Uni)   "Uni"
    checkUrlPiece  (Proxy :: Proxy F.Deci)  "Deci"
    checkUrlPiece  (Proxy :: Proxy F.Centi) "Centi"
    checkUrlPiece  (Proxy :: Proxy F.Milli) "Milli"
    checkUrlPiece  (Proxy :: Proxy F.Micro) "Micro"
    checkUrlPiece  (Proxy :: Proxy F.Nano)  "Nano"
    checkUrlPiece  (Proxy :: Proxy F.Pico)  "Pico"

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


uuidGen :: Gen UUID.UUID
uuidGen = UUID.fromWords <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- TODO: this generators don't generate full range items
localTimeGen :: Gen LocalTime
localTimeGen = LocalTime <$> arbitrary <*> timeOfDayGen

timeOfDayGen :: Gen TimeOfDay
timeOfDayGen = TimeOfDay
  <$> choose (0, 23)
  <*> choose (0, 59)
  <*> fmap (\x -> 0.1 * fromInteger x) (choose (0, 600))

zonedTimeGen :: Gen ZonedTime
zonedTimeGen = ZonedTime
    <$> localTimeGen -- Note: not arbitrary!
    <*> liftA3 TimeZone arbitrary arbitrary (vectorOf 3 (elements ['A'..'Z']))

utcTimeGen :: Gen UTCTime
utcTimeGen = UTCTime <$> arbitrary <*> fmap fromInteger (choose (0, 86400))

#if !MIN_VERSION_time(1,9,1)
nominalDiffTimeToSeconds :: NominalDiffTime -> F.Pico
nominalDiffTimeToSeconds = realToFrac

secondsToNominalDiffTime :: F.Pico -> NominalDiffTime
secondsToNominalDiffTime = realToFrac
#endif

nominalDiffTimeGen :: Gen NominalDiffTime
nominalDiffTimeGen = oneof 
    [ fromInteger <$> arbitrary              -- integral
    , secondsToNominalDiffTime <$> arbitrary -- with decimal part
    ]

setCookieGen :: Gen SetCookie
setCookieGen = do
    n <- TE.encodeUtf8 . T.pack . filter isAlphaNum <$> arbitrary
    v <- TE.encodeUtf8 . T.pack . filter isAlphaNum <$> arbitrary
    return $ defaultSetCookie { setCookieName = n, setCookieValue = v }
