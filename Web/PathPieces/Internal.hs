{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |
-- Convert Haskell values to and from route pieces.
module Web.PathPieces.Internal where

import Data.Monoid
import Data.Int
import Data.Word
import qualified Data.Text as S
import qualified Data.Text.Lazy as L
import Data.Version (Version)
import Data.Time (Day)
import Text.Read (readMaybe)

import Unsafe.Coerce

#if MIN_VERSION_base(4,8,0)
import Data.Void (Void)
#endif

import Web.HttpApiData
import Web.HttpApiData.Internal (parseMaybeTextData)

-- | Convert Haskell values to and from route piece.
class PathPiece s where
  -- | Convert from route piece.
  fromPathPiece :: S.Text -> Maybe s
  default fromPathPiece :: FromHttpApiData s => S.Text -> Maybe s
  fromPathPiece = either (const Nothing) Just . parseUrlPiece

  -- | Convert to route piece.
  toPathPiece :: s -> S.Text
  default toPathPiece :: ToHttpApiData s => s -> S.Text
  toPathPiece = toUrlPiece

instance PathPiece ()
instance PathPiece Char
instance PathPiece Bool
instance PathPiece Ordering
instance PathPiece Double
instance PathPiece Float
instance PathPiece Int
instance PathPiece Int8
instance PathPiece Int16
instance PathPiece Int32
instance PathPiece Int64
instance PathPiece Integer
instance PathPiece Word
instance PathPiece Word8
instance PathPiece Word16
instance PathPiece Word32
instance PathPiece Word64
instance PathPiece String
instance PathPiece S.Text
instance PathPiece L.Text
instance PathPiece Day
instance PathPiece Version

#if MIN_VERSION_base(4,8,0)
instance PathPiece Void
#endif

instance PathPiece All
instance PathPiece Any

instance PathPiece a => PathPiece (Dual a) where
  toPathPiece   = toPathPiece1
  fromPathPiece = fromPathPiece1

instance PathPiece a => PathPiece (Sum a) where
  toPathPiece   = toPathPiece1
  fromPathPiece = fromPathPiece1

instance PathPiece a => PathPiece (Product a) where
  toPathPiece   = toPathPiece1
  fromPathPiece = fromPathPiece1

instance PathPiece a => PathPiece (First a) where
  toPathPiece   = toPathPiece1
  fromPathPiece = fromPathPiece1

instance PathPiece a => PathPiece (Last a) where
  toPathPiece   = toPathPiece1
  fromPathPiece = fromPathPiece1

instance PathPiece a => PathPiece (Maybe a) where
  toPathPiece   = toPathPiece1
  fromPathPiece = fromPathPiece1

instance (PathPiece a, PathPiece b) => PathPiece (Either a b) where
  toPathPiece   = toPathPiece2
  fromPathPiece = fromPathPiece2

-- | Wrapped @'PathPiece'@ value.
newtype WrappedPathPiece a = WrappedPathPiece { unwrapPathPiece :: a }

instance PathPiece a => ToHttpApiData (WrappedPathPiece a) where
  toUrlPiece = toPathPiece . unwrapPathPiece

instance PathPiece a => FromHttpApiData (WrappedPathPiece a) where
  parseUrlPiece = fmap WrappedPathPiece . parseMaybeTextData fromPathPiece

-- | Convert value to route piece using @'PathPiece'@ instance for its parameter.
toPathPiece1 :: forall f a. (PathPiece a, ToHttpApiData (f (WrappedPathPiece a))) => f a -> S.Text
toPathPiece1 = toUrlPiece . (unsafeCoerce :: f a -> f (WrappedPathPiece a))

-- | Parse value from route piece using @'PathPiece'@ instance for its parameter.
fromPathPiece1 :: forall f a. (PathPiece a, FromHttpApiData (f (WrappedPathPiece a))) => S.Text -> Maybe (f a)
fromPathPiece1 = either (const Nothing) (Just . (unsafeCoerce :: f (WrappedPathPiece a) -> f a)) . parseUrlPiece

-- | Convert value to route piece using @'PathPiece'@ instance for its parameters.
toPathPiece2 :: forall f a b. (PathPiece a, PathPiece b, ToHttpApiData (f (WrappedPathPiece a) (WrappedPathPiece b))) => f a b -> S.Text
toPathPiece2 = toUrlPiece . (unsafeCoerce :: f a b -> f (WrappedPathPiece a) (WrappedPathPiece b))

-- | Parse value from route piece using @'PathPiece'@ instance for its parameters.
fromPathPiece2 :: forall f a b. (PathPiece a, PathPiece b, FromHttpApiData (f (WrappedPathPiece a) (WrappedPathPiece b))) => S.Text -> Maybe (f a b)
fromPathPiece2 = either (const Nothing) (Just . (unsafeCoerce :: f (WrappedPathPiece a) (WrappedPathPiece b) -> f a b)) . parseUrlPiece

-- | Convert Haskell values to and from sequence of route pieces.
class PathMultiPiece s where
  -- | Convert from sequence of route pieces.
  fromPathMultiPiece :: [S.Text] -> Maybe s
  -- | Convert to sequence of route pieces.
  toPathMultiPiece :: s -> [S.Text]

instance PathPiece a => PathMultiPiece [a] where
    fromPathMultiPiece = mapM fromPathPiece
    toPathMultiPiece = map toPathPiece

-- | A function for helping generate free 'PathPiece'
--   instances for enumeration data types 
--   that have derived 'Read' and 'Show' instances.
--   Intended to be used like this:
--
--   > data MyData = Foo | Bar | Baz
--   >   deriving (Read,Show)
--   > instance PathPiece MyData where
--   >   fromPathPiece = readFromPathPiece
--   >   toPathPiece = showToPathPiece
--
--  Since 0.2.1. 
readFromPathPiece :: Read s => S.Text -> Maybe s
readFromPathPiece = readMaybe . S.unpack

-- | See the documentation for 'readFromPathPiece'.
--
--  Since 0.2.1. 
showToPathPiece :: Show s => s -> S.Text
showToPathPiece = S.pack . show

