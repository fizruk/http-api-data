{-# LANGUAGE DefaultSignatures #-}
module Web.HttpApiData (
  ToHttpApiData (..),
  FromHttpApiData (..),
) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

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
