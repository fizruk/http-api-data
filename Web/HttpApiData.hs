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

  -- * Multiple URL pieces
  toUrlPieces,
  parseUrlPieces,

  -- * Multiple query params
  toQueryParams,
  parseQueryParams,

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
-- >>> parseUrlPieces ["true", "false", "undefined"] :: Either Text [Bool]
-- Left "could not parse: `undefined'"
--
-- Numbers:
--
-- >>> toQueryParam 45.2
-- "45.2"
-- >>> parseQueryParam "452" :: Either Text Int
-- Right 452
-- >>> toQueryParams [1..5]
-- ["1","2","3","4","5"]
-- >>> parseQueryParams ["127", "255"] :: Either Text [Int8]
-- Left "out of bounds: `255' (should be between -128 and 127)"
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

