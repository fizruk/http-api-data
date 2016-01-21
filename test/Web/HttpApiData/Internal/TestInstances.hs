{-# Language DeriveAnyClass       #-}
{-# Language DeriveGeneric        #-}
{-# Language ScopedTypeVariables  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Web.HttpApiData.Internal.TestInstances
   ( RandomCase(..)
   , SimpleRec(..)
   ) where

import           Control.Applicative
import           Data.Char
import qualified Data.Text                            as T
import qualified Data.Text.Lazy                       as L
import           Data.Time
import           Data.Version
import           GHC.Generics

import           Test.QuickCheck

import           Web.HttpApiData.Internal.HttpApiData
import           Web.HttpApiData.Internal.FormUrlEncoded

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary L.Text where
  arbitrary = L.pack <$> arbitrary

instance Arbitrary Day where
  arbitrary = liftA3 fromGregorian (fmap abs arbitrary) arbitrary arbitrary

instance Arbitrary LocalTime where
  arbitrary = LocalTime
    <$> arbitrary
    <*> liftA3 TimeOfDay (choose (0, 23)) (choose (0, 59)) (fromInteger <$> choose (0, 60))

instance Eq ZonedTime where
  ZonedTime t (TimeZone x _ _) == ZonedTime t' (TimeZone y _ _) = t == t' && x == y

instance Arbitrary ZonedTime where
  arbitrary = ZonedTime
    <$> arbitrary
    <*> liftA3 TimeZone arbitrary arbitrary (vectorOf 3 (elements ['A'..'Z']))

instance Arbitrary UTCTime where
  arbitrary = UTCTime <$> arbitrary <*> fmap fromInteger (choose (0, 86400))

instance Arbitrary NominalDiffTime where
  arbitrary = fromInteger <$> arbitrary

instance Arbitrary Version where
  arbitrary = (version . map abs) <$> nonempty
    where
      version branch = Version branch []
      nonempty = liftA2 (:) arbitrary arbitrary

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

data SimpleRec = SimpleRec { rec1 :: T.Text, rec2 :: T.Text }
  deriving (Eq, Show, Read, Generic)

instance ToFormUrlEncoded SimpleRec
instance FromFormUrlEncoded SimpleRec

instance Arbitrary SimpleRec where
  arbitrary = SimpleRec <$> arbitrary <*> arbitrary
