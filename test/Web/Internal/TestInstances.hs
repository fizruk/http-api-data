{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Web.Internal.TestInstances
   ( RandomCase(..)
   , SimpleRec(..)
   , NoEmptyKeyForm(..)
   ) where

import           Control.Applicative  -- for ghc < 9.6
import           Data.Char
import qualified Data.Map.Strict      as Map
import qualified Data.Text            as T
import           Data.Time.Compat
import           GHC.Exts             (fromList)
import           GHC.Generics

import Test.QuickCheck
import Test.QuickCheck.Instances ()

import Web.Internal.FormUrlEncoded
import Web.Internal.HttpApiData

instance Eq ZonedTime where
  ZonedTime t (TimeZone x _ _) == ZonedTime t' (TimeZone y _ _) = t == t' && x == y

instance Arbitrary Form where
  arbitrary = fromList <$> arbitrary

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

data SimpleRec = SimpleRec { rec1 :: T.Text, rec2 :: Int }
  deriving (Eq, Show, Read, Generic)

instance ToForm SimpleRec
instance FromForm SimpleRec

instance Arbitrary SimpleRec where
  arbitrary = SimpleRec <$> arbitrary <*> arbitrary

newtype NoEmptyKeyForm =
    NoEmptyKeyForm { unNoEmptyKeyForm :: Form }
    deriving Show

instance Arbitrary NoEmptyKeyForm where
  arbitrary = NoEmptyKeyForm . removeEmptyKeys <$> arbitrary
    where
      removeEmptyKeys (Form m) = Form (Map.delete "" m)
