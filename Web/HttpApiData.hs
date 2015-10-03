{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.HttpApiData (
  ToHttpApiData (..),
  FromHttpApiData (..),
  parseMaybeUrlPiece,
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

import Text.Read (readMaybe)

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
parseMaybeUrlPiece :: (Text -> Maybe a) -> (Text -> Either Text a)
parseMaybeUrlPiece parse input =
  case parse input of
    Nothing  -> Left ("parseUrlPiece: could not convert: `" <> input <> "'")
    Just val -> Right val

-- | Convert to URL piece using @'Show'@ instance.
showUrlPiece :: Show a => a -> Text
showUrlPiece = Text.pack . show

-- | Parse URL piece using @'Read'@ instance.
readMaybeUrlPiece :: Read a => Text -> Maybe a
readMaybeUrlPiece = readMaybe . Text.unpack

-- | Parse URL piece using @'Read'@ instance.
readEitherUrlPiece :: Read a => Text -> Either Text a
readEitherUrlPiece = parseMaybeUrlPiece readMaybeUrlPiece

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
instance ToHttpApiData [Char]   where toUrlPiece = Text.pack
instance ToHttpApiData Text     where toUrlPiece = id
instance ToHttpApiData TL.Text  where toUrlPiece = TL.toStrict

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
instance FromHttpApiData [Char]   where parseUrlPiece = Right . Text.unpack
instance FromHttpApiData Text     where parseUrlPiece = Right
instance FromHttpApiData TL.Text  where parseUrlPiece = Right . TL.fromStrict

