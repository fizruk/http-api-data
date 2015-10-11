-- |
-- Convert Haskell values to and from HTTP API data
-- such as URL pieces, headers and query parameters.
module Web.HttpApiData (
  -- * Examples
  -- $examples

  -- * Classes
  ToHttpApiData (..),
  FromHttpApiData (..),

  -- * @'Maybe'@ parsers
  parseUrlPieceMaybe,
  parseHeaderMaybe,
  parseQueryParamMaybe,

  -- * Prefix parsers
  parseUrlPieceWithPrefix,
  parseHeaderWithPrefix,
  parseQueryParamWithPrefix,

  -- * Other helpers
  showTextData,
  readTextData,
  parseBoundedTextData,
) where

import Web.HttpApiData.Internal

-- $setup
--
-- >>> :set -XOverloadedStrings
-- >>> import Control.Applicative
-- >>> import Data.Time
-- >>> import Data.Int
-- >>> import Data.Text (Text)
-- >>> import Data.Time (Day)
-- >>> import Data.Version

-- $examples
--
-- Booleans:
--
-- >>> toUrlPiece True
-- "true"
-- >>> parseUrlPiece "false" :: Either Text Bool
-- Right False
-- >>> parseUrlPiece "something else" :: Either Text Bool
-- Left "could not parse: `something else'"
--
-- Numbers:
--
-- >>> toUrlPiece 45.2
-- "45.2"
-- >>> parseUrlPiece "452" :: Either Text Int
-- Right 452
-- >>> parseUrlPiece "256" :: Either Text Int8
-- Left "out of bounds: `256' (should be between -128 and 127)"
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
