{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  parseMaybeTextData,
  showTextData,
  readMaybeTextData,
  readEitherTextData,
) where

import Control.Applicative
import Control.Arrow ((&&&))

import Data.Monoid
import Data.ByteString (ByteString)

import Data.Int
import Data.Word

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Text.Read (signed, decimal, rational, Reader)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L

import Data.Time (Day)
import Data.Version

#if MIN_VERSION_base(4,8,0)
import Data.Void
#endif

import Text.Read (readMaybe)
import Text.ParserCombinators.ReadP (readP_to_S)

-- $setup
--
-- >>> :set -XOverloadedStrings
-- >>> import Control.Applicative
-- >>> import Data.Time

-- | Convert value to HTTP API data.
class ToHttpApiData a where
  {-# MINIMAL toUrlPiece | toQueryParam #-}
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
  {-# MINIMAL parseUrlPiece | parseQueryParam #-}
  -- | Parse URL path piece.
  parseUrlPiece :: Text -> Either Text a
  parseUrlPiece = parseQueryParam

  -- | Parse HTTP header value.
  parseHeader :: ByteString -> Either Text a
  parseHeader = parseUrlPiece . decodeUtf8

  -- | Parse query param value.
  parseQueryParam :: Text -> Either Text a
  parseQueryParam = parseUrlPiece

-- | Default parsing error.
defaultParseError :: Text -> Either Text a
defaultParseError input = Left ("could not convert: `" <> input <> "'")

-- | Convert @'Maybe'@ parser into @'Either' 'Text'@ parser with default error message.
parseMaybeTextData :: (Text -> Maybe a) -> (Text -> Either Text a)
parseMaybeTextData parse input =
  case parse input of
    Nothing  -> defaultParseError input
    Just val -> Right val

-- | Convert to URL piece using @'Show'@ instance.
showTextData :: Show a => a -> Text
showTextData = T.toLower . T.pack . show

-- | Parse given text case insensitive and return the rest of the input.
--
-- >>> parseUrlPieceWithPrefix "Just " "just 10" :: Either Text Int
-- Right 10
-- >>> parseUrlPieceWithPrefix "Left " "left" :: Either Text Bool
-- Left "could not convert: `left'"
parseUrlPieceWithPrefix :: FromHttpApiData a => Text -> Text -> Either Text a
parseUrlPieceWithPrefix pattern input
  | T.toLower pattern == T.toLower prefix = parseUrlPiece rest
  | otherwise                             = defaultParseError input
  where
    (prefix, rest) = T.splitAt (T.length pattern) input

-- | Parse values case insensitively based on @'Show'@ instance.
--
-- >>> parseBoundedCaseInsensitiveTextData "true" :: Either Text Bool
-- Right True
-- >>> parseBoundedCaseInsensitiveTextData "FALSE" :: Either Text Bool
-- Right False
parseBoundedCaseInsensitiveTextData :: forall a. (Show a, Bounded a, Enum a) => Text -> Either Text a
parseBoundedCaseInsensitiveTextData = parseMaybeTextData (flip lookup values . T.toLower)
  where
    values = map (showTextData &&& id) [minBound..maxBound :: a]

-- | Parse URL piece using @'Read'@ instance.
readMaybeTextData :: Read a => Text -> Maybe a
readMaybeTextData = readMaybe . T.unpack

-- | Parse URL piece using @'Read'@ instance.
readEitherTextData :: Read a => Text -> Either Text a
readEitherTextData = parseMaybeTextData readMaybeTextData

-- | Run @'Reader'@ as HTTP API data parser.
runReader :: Reader a -> Text -> Either Text a
runReader reader input =
  case reader input of
    Left err          -> Left (T.pack err)
    Right (x, rest)
      | T.null rest -> Right x
      | otherwise   -> defaultParseError input

-- | Run @'Reader'@ to parse bounded integral value with bounds checking.
parseBounded :: forall a. (Bounded a, Integral a) => Reader Integer -> Text -> Either Text a
parseBounded reader input = do
  n <- runReader reader input
  if (n > h || n < l)
    then Left  ("out of bounds: `" <> input <> "' (should be between " <> T.pack (show l) <> " and " <> T.pack (show h) <> ")")
    else Right (fromInteger n)
  where
    l = toInteger (minBound :: a)
    h = toInteger (maxBound :: a)

-- |
-- >>> toUrlPiece ()
-- "_"
instance ToHttpApiData () where
  toUrlPiece () = "_"

instance ToHttpApiData Char     where toUrlPiece = T.singleton

-- |
-- >>> toUrlPiece (Version [1, 2, 3] [])
-- "1.2.3"
instance ToHttpApiData Version where
  toUrlPiece = T.pack . showVersion

#if MIN_VERSION_base(4,8,0)
instance ToHttpApiData Void where
  toUrlPiece = absurd
#endif

instance ToHttpApiData Bool     where toUrlPiece = showTextData
instance ToHttpApiData Ordering where toUrlPiece = showTextData
instance ToHttpApiData Double   where toUrlPiece = showTextData
instance ToHttpApiData Float    where toUrlPiece = showTextData
instance ToHttpApiData Int      where toUrlPiece = showTextData
instance ToHttpApiData Int8     where toUrlPiece = showTextData
instance ToHttpApiData Int16    where toUrlPiece = showTextData
instance ToHttpApiData Int32    where toUrlPiece = showTextData
instance ToHttpApiData Int64    where toUrlPiece = showTextData
instance ToHttpApiData Integer  where toUrlPiece = showTextData
instance ToHttpApiData Word     where toUrlPiece = showTextData
instance ToHttpApiData Word8    where toUrlPiece = showTextData
instance ToHttpApiData Word16   where toUrlPiece = showTextData
instance ToHttpApiData Word32   where toUrlPiece = showTextData
instance ToHttpApiData Word64   where toUrlPiece = showTextData
instance ToHttpApiData String   where toUrlPiece = T.pack
instance ToHttpApiData Text     where toUrlPiece = id
instance ToHttpApiData L.Text   where toUrlPiece = L.toStrict
instance ToHttpApiData Day      where toUrlPiece = showTextData

instance ToHttpApiData All where toUrlPiece = toUrlPiece . getAll
instance ToHttpApiData Any where toUrlPiece = toUrlPiece . getAny

instance ToHttpApiData a => ToHttpApiData (Dual a)    where toUrlPiece = toUrlPiece . getDual
instance ToHttpApiData a => ToHttpApiData (Sum a)     where toUrlPiece = toUrlPiece . getSum
instance ToHttpApiData a => ToHttpApiData (Product a) where toUrlPiece = toUrlPiece . getProduct
instance ToHttpApiData a => ToHttpApiData (First a)   where toUrlPiece = toUrlPiece . getFirst
instance ToHttpApiData a => ToHttpApiData (Last a)    where toUrlPiece = toUrlPiece . getLast

-- |
-- >>> toUrlPiece (Just "Hello")
-- "just Hello"
instance ToHttpApiData a => ToHttpApiData (Maybe a) where
  toUrlPiece (Just x) = "just " <> toUrlPiece x
  toUrlPiece Nothing  = "nothing"

-- |
-- >>> toUrlPiece (Left "err" :: Either String Int)
-- "left err"
-- >>> toUrlPiece (Right 3 :: Either String Int)
-- "right 3"
instance (ToHttpApiData a, ToHttpApiData b) => ToHttpApiData (Either a b) where
  toUrlPiece (Left x)  = "left " <> toUrlPiece x
  toUrlPiece (Right x) = "right " <> toUrlPiece x

-- |
-- >>> parseUrlPiece "_" :: Either Text ()
-- Right ()
instance FromHttpApiData () where
  parseUrlPiece "_" = return ()
  parseUrlPiece s   = defaultParseError s

instance FromHttpApiData Char where
  parseUrlPiece s =
    case T.uncons s of
      Just (c, s') | T.null s' -> return c
      _                        -> defaultParseError s

-- |
-- >>> showVersion <$> parseUrlPiece "1.2.3"
-- Right "1.2.3"
instance FromHttpApiData Version where
  parseUrlPiece s =
    case reverse (readP_to_S parseVersion (T.unpack s)) of
      ((x, ""):_) -> return x
      _           -> defaultParseError s

#if MIN_VERSION_base(4,8,0)
instance FromHttpApiData Void where
  parseUrlPiece _ = Left "Void cannot be parsed!"
#endif

instance FromHttpApiData Bool     where parseUrlPiece = parseBoundedCaseInsensitiveTextData
instance FromHttpApiData Ordering where parseUrlPiece = parseBoundedCaseInsensitiveTextData
instance FromHttpApiData Double   where parseUrlPiece = runReader rational
instance FromHttpApiData Float    where parseUrlPiece = runReader rational
instance FromHttpApiData Int      where parseUrlPiece = parseBounded (signed decimal)
instance FromHttpApiData Int8     where parseUrlPiece = parseBounded (signed decimal)
instance FromHttpApiData Int16    where parseUrlPiece = parseBounded (signed decimal)
instance FromHttpApiData Int32    where parseUrlPiece = parseBounded (signed decimal)
instance FromHttpApiData Int64    where parseUrlPiece = parseBounded (signed decimal)
instance FromHttpApiData Integer  where parseUrlPiece = runReader (signed decimal)
instance FromHttpApiData Word     where parseUrlPiece = parseBounded decimal
instance FromHttpApiData Word8    where parseUrlPiece = parseBounded decimal
instance FromHttpApiData Word16   where parseUrlPiece = parseBounded decimal
instance FromHttpApiData Word32   where parseUrlPiece = parseBounded decimal
instance FromHttpApiData Word64   where parseUrlPiece = parseBounded decimal
instance FromHttpApiData String   where parseUrlPiece = Right . T.unpack
instance FromHttpApiData Text     where parseUrlPiece = Right
instance FromHttpApiData L.Text   where parseUrlPiece = Right . L.fromStrict
instance FromHttpApiData Day      where parseUrlPiece = readEitherTextData

instance FromHttpApiData All where parseUrlPiece = fmap All . parseUrlPiece
instance FromHttpApiData Any where parseUrlPiece = fmap Any . parseUrlPiece

instance FromHttpApiData a => FromHttpApiData (Dual a)    where parseUrlPiece = fmap Dual    . parseUrlPiece
instance FromHttpApiData a => FromHttpApiData (Sum a)     where parseUrlPiece = fmap Sum     . parseUrlPiece
instance FromHttpApiData a => FromHttpApiData (Product a) where parseUrlPiece = fmap Product . parseUrlPiece
instance FromHttpApiData a => FromHttpApiData (First a)   where parseUrlPiece = fmap First   . parseUrlPiece
instance FromHttpApiData a => FromHttpApiData (Last a)    where parseUrlPiece = fmap Last    . parseUrlPiece

-- |
-- >>> parseUrlPiece "Just 123" :: Either Text (Maybe Int)
-- Right (Just 123)
instance FromHttpApiData a => FromHttpApiData (Maybe a) where
  parseUrlPiece s
    | T.toLower (T.take 7 s) == "nothing" = return Nothing
    | otherwise                           = Just <$> parseUrlPieceWithPrefix "Just " s

-- |
-- >>> parseUrlPiece "Right 123" :: Either Text (Either String Int)
-- Right (Right 123)
instance (FromHttpApiData a, FromHttpApiData b) => FromHttpApiData (Either a b) where
  parseUrlPiece s =
        Right <$> parseUrlPieceWithPrefix "Right " s
    <!> Left  <$> parseUrlPieceWithPrefix "Left " s
    where
      infixl 3 <!>
      Left _ <!> y = y
      x      <!> _ = x

-- $examples
--
-- Booleans:
--
-- >>> toUrlPiece True
-- "true"
-- >>> parseUrlPiece "false" :: Either Text Bool
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
