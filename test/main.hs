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

import Web.PathPieces

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

(<=>) :: Eq a => (a -> b) -> (b -> Maybe a) -> a -> Bool
(f <=> g) x = g (f x) == Just x

data Proxy a = Proxy

checkPathPiece :: forall a. (Eq a, PathPiece a, Show a, Arbitrary a) => Proxy a -> String -> Spec
checkPathPiece _ name = prop ("toPathPiece <=> fromPathPiece " ++ name) (toPathPiece <=> fromPathPiece :: a -> Bool)

checkPathMultiPiece :: forall a. (Eq a, PathMultiPiece a, Show a, Arbitrary a) => Proxy a -> String -> Spec
checkPathMultiPiece _ name = prop ("toPathMultiPiece <=> fromPathMultiPiece " ++ name) (toPathMultiPiece <=> fromPathMultiPiece :: a -> Bool)

data RandomCase a = RandomCase [Bool] a

instance PathPiece a => Show (RandomCase a) where
  show rc@(RandomCase _ x) = show (toPathPiece rc) ++ " (original: " ++ show (toPathPiece x) ++ ")"

instance Eq a => Eq (RandomCase a) where
  RandomCase _ x == RandomCase _ y = x == y

instance Arbitrary a => Arbitrary (RandomCase a) where
  arbitrary = liftA2 RandomCase nonempty arbitrary
    where
      nonempty = liftA2 (:) arbitrary arbitrary

instance PathPiece a => PathPiece (RandomCase a) where
  toPathPiece (RandomCase us x) = T.pack (zipWith (\u -> if u then toUpper else toLower) (cycle us) (T.unpack (toPathPiece x)))
  fromPathPiece s = RandomCase [] <$> fromPathPiece s

-- | Check case insensitivity for @fromPathPiece@.
checkPathPieceI :: forall a. (Eq a, PathPiece a, Show a, Arbitrary a) => Proxy a -> String -> Spec
checkPathPieceI _ = checkPathPiece (Proxy :: Proxy (RandomCase a))

spec :: Spec
spec = do
  describe "PathPiece" $ do
    checkPathPiece  (Proxy :: Proxy ())        "()"
    checkPathPiece  (Proxy :: Proxy Char)      "Char"
    checkPathPieceI (Proxy :: Proxy Bool)      "Bool"
    checkPathPieceI (Proxy :: Proxy Ordering)  "Ordering"
    checkPathPiece  (Proxy :: Proxy Int)       "Int"
    checkPathPiece  (Proxy :: Proxy Int8)      "Int8"
    checkPathPiece  (Proxy :: Proxy Int16)     "Int16"
    checkPathPiece  (Proxy :: Proxy Int32)     "Int32"
    checkPathPiece  (Proxy :: Proxy Int64)     "Int64"
    checkPathPiece  (Proxy :: Proxy Integer)   "Integer"
    checkPathPiece  (Proxy :: Proxy Word)      "Word"
    checkPathPiece  (Proxy :: Proxy Word8)     "Word8"
    checkPathPiece  (Proxy :: Proxy Word16)    "Word16"
    checkPathPiece  (Proxy :: Proxy Word32)    "Word32"
    checkPathPiece  (Proxy :: Proxy Word64)    "Word64"
    checkPathPiece  (Proxy :: Proxy String)    "String"
    checkPathPiece  (Proxy :: Proxy T.Text)    "Text.Strict"
    checkPathPiece  (Proxy :: Proxy L.Text)    "Text.Lazy"
    checkPathPiece  (Proxy :: Proxy Day)       "Day"
    checkPathPiece  (Proxy :: Proxy Version)   "Version"

    checkPathPiece  (Proxy :: Proxy (Maybe String))            "Maybe String"
    checkPathPieceI (Proxy :: Proxy (Maybe Integer))           "Maybe Integer"
    checkPathPiece  (Proxy :: Proxy (Either Integer T.Text))   "Either Integer Text"
    checkPathPieceI (Proxy :: Proxy (Either Version Day))      "Either Version Day"

  describe "PathMultiPiece" $ do
    checkPathMultiPiece (Proxy :: Proxy [String])   "[String]"
    checkPathMultiPiece (Proxy :: Proxy [T.Text])   "[Text]"

  it "bad integers are rejected" $ do
    fromPathPiece (T.pack "123hello") `shouldBe` (Nothing :: Maybe Int)

  it "bounds checking works" $ do
    fromPathPiece (T.pack "256") `shouldBe` (Nothing :: Maybe Int8)
    fromPathPiece (T.pack "-10") `shouldBe` (Nothing :: Maybe Word)

