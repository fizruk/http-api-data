{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.HttpApiData (
  ToHttpApiData (..),
  FromHttpApiData (..),
  parseMaybeUrlPiece,
  showUrlPiece,
  readMaybeUrlPiece,
  readEitherUrlPiece,
) where

import Data.Monoid
import Data.ByteString (ByteString)

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Text as Text

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

