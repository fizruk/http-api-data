{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |
-- Convert Haskell values to and from route pieces.
module Web.PathPieces (
  PathPiece (..),
  PathMultiPiece (..),
  readFromPathPiece,
  showToPathPiece,
) where

import Data.Int
import Data.Word
import qualified Data.Text as S
import qualified Data.Text.Lazy as L
import Data.Time (Day)
import Text.Read (readMaybe)

import Web.HttpApiData

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
instance PathPiece Bool
instance PathPiece Double
instance PathPiece Float
instance PathPiece Int
instance PathPiece Int8
instance PathPiece Int16
instance PathPiece Int32
instance PathPiece Int64
instance PathPiece Word
instance PathPiece Word8
instance PathPiece Word16
instance PathPiece Word32
instance PathPiece Word64
instance PathPiece String
instance PathPiece S.Text
instance PathPiece L.Text
instance PathPiece Day

instance (PathPiece a) => PathPiece (Maybe a) where
    fromPathPiece s = case S.stripPrefix "Just " s of
        Just r -> Just `fmap` fromPathPiece r
        _ -> case s of
            "Nothing" -> Just Nothing
            _ -> Nothing
    toPathPiece m = case m of
        Just s -> "Just " `S.append` toPathPiece s
        _ -> "Nothing"

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

