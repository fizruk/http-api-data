{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |
-- Convert Haskell values to and from HTTP API data
-- such as URL pieces, headers and query parameters.
module Web.HttpApiData (
  -- * Examples
  -- $examples

  -- * Classes
  ToHttpApiData (..),
  FromHttpApiData (..),
  -- * Utility functions
  parseMaybeHttpApiData,
  showUrlPiece,
  readMaybeUrlPiece,
  readEitherUrlPiece,
) where

import Control.Arrow ((+++))

import Data.Monoid
import Data.ByteString (ByteString)

import Data.Int
import Data.Word

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Text.Read (signed, decimal, rational, Reader)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TL

import Data.Time (Day)

import Text.Read (readMaybe)

-- $setup
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.Time

-- | Convert value to HTTP API data.
class ToHttpApiData a where
  -- | Convert to URL path piece.
  toUrlPiece :: a -> Text
  toUrlPiece = toQueryParam

  -- | Convert to HTTP header value.
  toHeader :: a -> ByteString
  toHeader = encodeUtf8 . toUrlPiece

  -- | Convert to query param value.
  toQueryParam :: a -> Text
  toQueryParam = toUrlPiece

-- | Convert value from HTTP API data.
class FromHttpApiData a where
  -- | Parse URL path piece.
  parseUrlPiece :: Text -> Either Text a
  parseUrlPiece = parseQueryParam

  -- | Parse HTTP header value.
  parseHeader :: ByteString -> Either Text a
  parseHeader = parseUrlPiece . decodeUtf8

  -- | Parse query param value.
  parseQueryParam :: Text -> Either Text a
  parseQueryParam = parseUrlPiece

-- | Convert @'Maybe'@ parser into @'Either' 'Text'@ parser with default error message.
parseMaybeHttpApiData :: (Text -> Maybe a) -> (Text -> Either Text a)
parseMaybeHttpApiData parse input =
  case parse input of
    Nothing  -> Left ("could not convert: `" <> input <> "'")
    Just val -> Right val

-- | Convert to URL piece using @'Show'@ instance.
showUrlPiece :: Show a => a -> Text
showUrlPiece = Text.pack . show

-- | Parse URL piece using @'Read'@ instance.
readMaybeUrlPiece :: Read a => Text -> Maybe a
readMaybeUrlPiece = readMaybe . Text.unpack

-- | Parse URL piece using @'Read'@ instance.
readEitherUrlPiece :: Read a => Text -> Either Text a
readEitherUrlPiece = parseMaybeHttpApiData readMaybeUrlPiece

-- | Run @'Reader'@ as HTTP API data parser.
runReader :: Reader a -> Text -> Either Text a
runReader reader = (Text.pack +++ fst) . reader

instance ToHttpApiData Bool     where toUrlPiece = showUrlPiece
instance ToHttpApiData Double   where toUrlPiece = showUrlPiece
instance ToHttpApiData Float    where toUrlPiece = showUrlPiece
instance ToHttpApiData Int      where toUrlPiece = showUrlPiece
instance ToHttpApiData Int8     where toUrlPiece = showUrlPiece
instance ToHttpApiData Int16    where toUrlPiece = showUrlPiece
instance ToHttpApiData Int32    where toUrlPiece = showUrlPiece
instance ToHttpApiData Int64    where toUrlPiece = showUrlPiece
instance ToHttpApiData Integer  where toUrlPiece = showUrlPiece
instance ToHttpApiData Word     where toUrlPiece = showUrlPiece
instance ToHttpApiData Word8    where toUrlPiece = showUrlPiece
instance ToHttpApiData Word16   where toUrlPiece = showUrlPiece
instance ToHttpApiData Word32   where toUrlPiece = showUrlPiece
instance ToHttpApiData Word64   where toUrlPiece = showUrlPiece
instance ToHttpApiData String   where toUrlPiece = Text.pack
instance ToHttpApiData Text     where toUrlPiece = id
instance ToHttpApiData TL.Text  where toUrlPiece = TL.toStrict
instance ToHttpApiData Day      where toUrlPiece = showUrlPiece

instance FromHttpApiData Bool     where parseUrlPiece = readEitherUrlPiece
instance FromHttpApiData Double   where parseUrlPiece = runReader rational
instance FromHttpApiData Float    where parseUrlPiece = runReader rational
instance FromHttpApiData Int      where parseUrlPiece = runReader (signed decimal)
instance FromHttpApiData Int8     where parseUrlPiece = runReader (signed decimal)
instance FromHttpApiData Int16    where parseUrlPiece = runReader (signed decimal)
instance FromHttpApiData Int32    where parseUrlPiece = runReader (signed decimal)
instance FromHttpApiData Int64    where parseUrlPiece = runReader (signed decimal)
instance FromHttpApiData Integer  where parseUrlPiece = runReader (signed decimal)
instance FromHttpApiData Word     where parseUrlPiece = runReader decimal
instance FromHttpApiData Word8    where parseUrlPiece = runReader decimal
instance FromHttpApiData Word16   where parseUrlPiece = runReader decimal
instance FromHttpApiData Word32   where parseUrlPiece = runReader decimal
instance FromHttpApiData Word64   where parseUrlPiece = runReader decimal
instance FromHttpApiData String   where parseUrlPiece = Right . Text.unpack
instance FromHttpApiData Text     where parseUrlPiece = Right
instance FromHttpApiData TL.Text  where parseUrlPiece = Right . TL.fromStrict
instance FromHttpApiData Day      where parseUrlPiece = readEitherUrlPiece

-- $examples
--
-- Booleans:
--
-- >>> toUrlPiece True
-- "True"
-- >>> parseUrlPiece "False" :: Either Text Bool
-- Right False
-- >>> parseUrlPiece "something else" :: Either Text Bool
-- Left "could not convert: `something else'"
--
-- Numbers:
--
-- >>> toUrlPiece 45.2
-- "45.2"
-- >>> parseUrlPiece "452" :: Either Text Int
-- Right 452
--
-- Strings:
--
-- >>> toHeader "hello"
-- "hello"
-- >>> parseHeader "world" :: Either Text String
-- Right "world"
--
-- Calendar day:
--
-- >>> toQueryParam (fromGregorian 2015 10 03)
-- "2015-10-03"
-- >>> toGregorian <$> parseQueryParam "2016-12-01"
-- Right (2016,12,1)
