{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |
-- Convert Haskell values to and from HTTP API data
-- such as URL pieces, headers and query parameters.
module Web.Internal.HttpApiData where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
import           Data.Foldable                (Foldable)
import           Data.Traversable             (Traversable (traverse))
#endif

import           Control.Arrow                (left, (&&&))
import           Control.Monad                ((<=<))

import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS
import           Data.Monoid

import           Data.Int
import           Data.Word

import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Text.Encoding           (decodeUtf8', encodeUtf8)
import qualified Data.Text.Lazy               as L
import           Data.Text.Read               (Reader, decimal, rational,
                                               signed)

import           Data.Time
import           Data.Time.Locale.Compat
import           Data.Version

#if MIN_VERSION_base(4,8,0)
import           Data.Void
import           Numeric.Natural
#endif

import           Text.ParserCombinators.ReadP (readP_to_S)
import           Text.Read                    (readMaybe)

#if USE_TEXT_SHOW
import           TextShow                     (TextShow, showt)
#endif

import qualified Data.UUID.Types              as UUID

import qualified Data.ByteString.Builder      as BS
import           Data.Data                    (Data)
import           Data.Typeable                (Typeable)
import qualified Network.HTTP.Types           as H

import qualified Data.Attoparsec.Text         as Atto
import qualified Data.Attoparsec.Time         as Atto

import           Web.Cookie                   (SetCookie, parseSetCookie,
                                               renderSetCookie)


-- $setup
-- >>> data BasicAuthToken = BasicAuthToken Text deriving (Show)
-- >>> instance FromHttpApiData BasicAuthToken where parseHeader h = BasicAuthToken <$> parseHeaderWithPrefix "Basic " h; parseQueryParam p = BasicAuthToken <$> parseQueryParam p

-- | Convert value to HTTP API data.
--
-- __WARNING__: Do not derive this using @DeriveAnyClass@ as the generated
-- instance will loop indefinitely.
class ToHttpApiData a where
  {-# MINIMAL toUrlPiece | toQueryParam #-}
  -- | Convert to URL path piece.
  toUrlPiece :: a -> Text
  toUrlPiece = toQueryParam

  -- | Convert to a URL path piece, making sure to encode any special chars.
  -- The default definition uses 'H.encodePathSegmentsRelative',
  -- but this may be overriden with a more efficient version.
  toEncodedUrlPiece :: a -> BS.Builder
  toEncodedUrlPiece = H.encodePathSegmentsRelative . (:[]) . toUrlPiece

  -- | Convert to HTTP header value.
  toHeader :: a -> ByteString
  toHeader = encodeUtf8 . toUrlPiece

  -- | Convert to query param value.
  toQueryParam :: a -> Text
  toQueryParam = toUrlPiece

-- | Parse value from HTTP API data.
--
-- __WARNING__: Do not derive this using @DeriveAnyClass@ as the generated
-- instance will loop indefinitely.
class FromHttpApiData a where
  {-# MINIMAL parseUrlPiece | parseQueryParam #-}
  -- | Parse URL path piece.
  parseUrlPiece :: Text -> Either Text a
  parseUrlPiece = parseQueryParam

  -- | Parse HTTP header value.
  parseHeader :: ByteString -> Either Text a
  parseHeader = parseUrlPiece <=< (left (T.pack . show) . decodeUtf8')

  -- | Parse query param value.
  parseQueryParam :: Text -> Either Text a
  parseQueryParam = parseUrlPiece

-- | Convert multiple values to a list of URL pieces.
--
-- >>> toUrlPieces [1, 2, 3] :: [Text]
-- ["1","2","3"]
toUrlPieces :: (Functor t, ToHttpApiData a) => t a -> t Text
toUrlPieces = fmap toUrlPiece

-- | Parse multiple URL pieces.
--
-- >>> parseUrlPieces ["true", "false"] :: Either Text [Bool]
-- Right [True,False]
-- >>> parseUrlPieces ["123", "hello", "world"] :: Either Text [Int]
-- Left "could not parse: `hello' (input does not start with a digit)"
parseUrlPieces :: (Traversable t, FromHttpApiData a) => t Text -> Either Text (t a)
parseUrlPieces = traverse parseUrlPiece

-- | Convert multiple values to a list of query parameter values.
--
-- >>> toQueryParams [fromGregorian 2015 10 03, fromGregorian 2015 12 01] :: [Text]
-- ["2015-10-03","2015-12-01"]
toQueryParams :: (Functor t, ToHttpApiData a) => t a -> t Text
toQueryParams = fmap toQueryParam

-- | Parse multiple query parameters.
--
-- >>> parseQueryParams ["1", "2", "3"] :: Either Text [Int]
-- Right [1,2,3]
-- >>> parseQueryParams ["64", "128", "256"] :: Either Text [Word8]
-- Left "out of bounds: `256' (should be between 0 and 255)"
parseQueryParams :: (Traversable t, FromHttpApiData a) => t Text -> Either Text (t a)
parseQueryParams = traverse parseQueryParam

-- | Parse URL path piece in a @'Maybe'@.
--
-- >>> parseUrlPieceMaybe "12" :: Maybe Int
-- Just 12
parseUrlPieceMaybe :: FromHttpApiData a => Text -> Maybe a
parseUrlPieceMaybe = either (const Nothing) Just . parseUrlPiece

-- | Parse HTTP header value in a @'Maybe'@.
--
-- >>> parseHeaderMaybe "hello" :: Maybe Text
-- Just "hello"
parseHeaderMaybe :: FromHttpApiData a => ByteString -> Maybe a
parseHeaderMaybe = either (const Nothing) Just . parseHeader

-- | Parse query param value in a @'Maybe'@.
--
-- >>> parseQueryParamMaybe "true" :: Maybe Bool
-- Just True
parseQueryParamMaybe :: FromHttpApiData a => Text -> Maybe a
parseQueryParamMaybe = either (const Nothing) Just . parseQueryParam

-- | Default parsing error.
defaultParseError :: Text -> Either Text a
defaultParseError input = Left ("could not parse: `" <> input <> "'")

-- | Convert @'Maybe'@ parser into @'Either' 'Text'@ parser with default error message.
parseMaybeTextData :: (Text -> Maybe a) -> (Text -> Either Text a)
parseMaybeTextData parse input =
  case parse input of
    Nothing  -> defaultParseError input
    Just val -> Right val

#if USE_TEXT_SHOW
-- | /Lower case/.
--
-- Convert to URL piece using @'TextShow'@ instance.
-- The result is always lower cased.
--
-- >>> showTextData True
-- "true"
--
-- This can be used as a default implementation for enumeration types:
--
-- @
-- data MyData = Foo | Bar | Baz deriving (Generic)
--
-- instance TextShow MyData where
--   showt = genericShowt
--
-- instance ToHttpApiData MyData where
--   toUrlPiece = showTextData
-- @
showTextData :: TextShow a => a -> Text
showTextData = T.toLower . showt
#else
-- | /Lower case/.
--
-- Convert to URL piece using @'Show'@ instance.
-- The result is always lower cased.
--
-- >>> showTextData True
-- "true"
--
-- This can be used as a default implementation for enumeration types:
--
-- >>> data MyData = Foo | Bar | Baz deriving (Show)
-- >>> instance ToHttpApiData MyData where toUrlPiece = showTextData
-- >>> toUrlPiece Foo
-- "foo"
showTextData :: Show a => a -> Text
showTextData = T.toLower . showt

-- | Like @'show'@, but returns @'Text'@.
showt :: Show a => a -> Text
showt = T.pack . show
#endif

-- | /Case insensitive/.
--
-- Parse given text case insensitive and then parse the rest of the input
-- using @'parseUrlPiece'@.
--
-- >>> parseUrlPieceWithPrefix "Just " "just 10" :: Either Text Int
-- Right 10
-- >>> parseUrlPieceWithPrefix "Left " "left" :: Either Text Bool
-- Left "could not parse: `left'"
--
-- This can be used to implement @'FromHttpApiData'@ for single field constructors:
--
-- >>> data Foo = Foo Int deriving (Show)
-- >>> instance FromHttpApiData Foo where parseUrlPiece s = Foo <$> parseUrlPieceWithPrefix "Foo " s
-- >>> parseUrlPiece "foo 1" :: Either Text Foo
-- Right (Foo 1)
parseUrlPieceWithPrefix :: FromHttpApiData a => Text -> Text -> Either Text a
parseUrlPieceWithPrefix pattern input
  | T.toLower pattern == T.toLower prefix = parseUrlPiece rest
  | otherwise                             = defaultParseError input
  where
    (prefix, rest) = T.splitAt (T.length pattern) input

-- | Parse given bytestring then parse the rest of the input using @'parseHeader'@.
--
-- @
-- data BasicAuthToken = BasicAuthToken Text deriving (Show)
--
-- instance FromHttpApiData BasicAuthToken where
--   parseHeader h     = BasicAuthToken \<$\> parseHeaderWithPrefix "Basic " h
--   parseQueryParam p = BasicAuthToken \<$\> parseQueryParam p
-- @
--
-- >>> parseHeader "Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==" :: Either Text BasicAuthToken
-- Right (BasicAuthToken "QWxhZGRpbjpvcGVuIHNlc2FtZQ==")
parseHeaderWithPrefix :: FromHttpApiData a => ByteString -> ByteString -> Either Text a
parseHeaderWithPrefix pattern input
  | pattern `BS.isPrefixOf` input = parseHeader (BS.drop (BS.length pattern) input)
  | otherwise                     = defaultParseError (showt input)

-- | /Case insensitive/.
--
-- Parse given text case insensitive and then parse the rest of the input
-- using @'parseQueryParam'@.
--
-- >>> parseQueryParamWithPrefix "z" "z10" :: Either Text Int
-- Right 10
parseQueryParamWithPrefix :: FromHttpApiData a => Text -> Text -> Either Text a
parseQueryParamWithPrefix pattern input
  | T.toLower pattern == T.toLower prefix = parseQueryParam rest
  | otherwise                             = defaultParseError input
  where
    (prefix, rest) = T.splitAt (T.length pattern) input

#if USE_TEXT_SHOW
-- | /Case insensitive/.
--
-- Parse values case insensitively based on @'TextShow'@ instance.
--
-- >>> parseBoundedTextData "true" :: Either Text Bool
-- Right True
-- >>> parseBoundedTextData "FALSE" :: Either Text Bool
-- Right False
--
-- This can be used as a default implementation for enumeration types:
--
-- @
-- data MyData = Foo | Bar | Baz deriving (Show, Bounded, Enum, Generic)
--
-- instance TextShow MyData where
--   showt = genericShowt
--
-- instance FromHttpApiData MyData where
--   parseUrlPiece = parseBoundedTextData
-- @
parseBoundedTextData :: (TextShow a, Bounded a, Enum a) => Text -> Either Text a
#else
-- | /Case insensitive/.
--
-- Parse values case insensitively based on @'Show'@ instance.
--
-- >>> parseBoundedTextData "true" :: Either Text Bool
-- Right True
-- >>> parseBoundedTextData "FALSE" :: Either Text Bool
-- Right False
--
-- This can be used as a default implementation for enumeration types:
--
-- >>> data MyData = Foo | Bar | Baz deriving (Show, Bounded, Enum)
-- >>> instance FromHttpApiData MyData where parseUrlPiece = parseBoundedTextData
-- >>> parseUrlPiece "foo" :: Either Text MyData
-- Right Foo
parseBoundedTextData :: (Show a, Bounded a, Enum a) => Text -> Either Text a
#endif
parseBoundedTextData = parseBoundedEnumOfI showTextData

-- | Lookup values based on a precalculated mapping of their representations.
lookupBoundedEnumOf :: (Bounded a, Enum a, Eq b) => (a -> b) -> b -> Maybe a
lookupBoundedEnumOf f = flip lookup (map (f &&& id) [minBound..maxBound])

-- | Parse values based on a precalculated mapping of their @'Text'@ representation.
--
-- >>> parseBoundedEnumOf toUrlPiece "true" :: Either Text Bool
-- Right True
--
-- For case sensitive parser see 'parseBoundedEnumOfI'.
parseBoundedEnumOf :: (Bounded a, Enum a) => (a -> Text) -> Text -> Either Text a
parseBoundedEnumOf = parseMaybeTextData . lookupBoundedEnumOf

-- | /Case insensitive/.
--
-- Parse values case insensitively based on a precalculated mapping
-- of their @'Text'@ representations.
--
-- >>> parseBoundedEnumOfI toUrlPiece "FALSE" :: Either Text Bool
-- Right False
--
-- For case sensitive parser see 'parseBoundedEnumOf'.
parseBoundedEnumOfI :: (Bounded a, Enum a) => (a -> Text) -> Text -> Either Text a
parseBoundedEnumOfI f = parseBoundedEnumOf (T.toLower . f) . T.toLower

-- | /Case insensitive/.
--
-- Parse values case insensitively based on @'ToHttpApiData'@ instance.
-- Uses @'toUrlPiece'@ to get possible values.
parseBoundedUrlPiece :: (ToHttpApiData a, Bounded a, Enum a) => Text -> Either Text a
parseBoundedUrlPiece = parseBoundedEnumOfI toUrlPiece

-- | /Case insensitive/.
--
-- Parse values case insensitively based on @'ToHttpApiData'@ instance.
-- Uses @'toQueryParam'@ to get possible values.
parseBoundedQueryParam :: (ToHttpApiData a, Bounded a, Enum a) => Text -> Either Text a
parseBoundedQueryParam = parseBoundedEnumOfI toQueryParam

-- | Parse values based on @'ToHttpApiData'@ instance.
-- Uses @'toHeader'@ to get possible values.
parseBoundedHeader :: (ToHttpApiData a, Bounded a, Enum a) => ByteString -> Either Text a
parseBoundedHeader bs = case lookupBoundedEnumOf toHeader bs of
  Nothing -> defaultParseError $ T.pack $ show bs
  Just x  -> return x

-- | Parse URL piece using @'Read'@ instance.
--
-- Use for types which do not involve letters:
--
-- >>> readTextData "1991-06-02" :: Either Text Day
-- Right 1991-06-02
--
-- This parser is case sensitive and will not match @'showTextData'@
-- in presence of letters:
--
-- >>> readTextData (showTextData True) :: Either Text Bool
-- Left "could not parse: `true'"
--
-- See @'parseBoundedTextData'@.
readTextData :: Read a => Text -> Either Text a
readTextData = parseMaybeTextData (readMaybe . T.unpack)

-- | Run @'Reader'@ as HTTP API data parser.
runReader :: Reader a -> Text -> Either Text a
runReader reader input =
  case reader input of
    Left err          -> Left ("could not parse: `" <> input <> "' (" <> T.pack err <> ")")
    Right (x, rest)
      | T.null rest -> Right x
      | otherwise   -> defaultParseError input

-- | Run @'Reader'@ to parse bounded integral value with bounds checking.
--
-- >>> parseBounded decimal "256" :: Either Text Word8
-- Left "out of bounds: `256' (should be between 0 and 255)"
parseBounded :: forall a. (Bounded a, Integral a) => Reader Integer -> Text -> Either Text a
parseBounded reader input = do
  n <- runReader reader input
  if (n > h || n < l)
    then Left  ("out of bounds: `" <> input <> "' (should be between " <> showt l <> " and " <> showt h <> ")")
    else Right (fromInteger n)
  where
    l = toInteger (minBound :: a)
    h = toInteger (maxBound :: a)

-- | Convert to a URL-encoded path piece using 'toUrlPiece'.
-- /Note/: this function does not check if the result contains unescaped characters!
-- This function can be used to override 'toEncodedUrlPiece' as a more efficient implementation
-- when the resulting URL piece /never/ has to be escaped.
unsafeToEncodedUrlPiece :: ToHttpApiData a => a -> BS.Builder
unsafeToEncodedUrlPiece = BS.byteString . encodeUtf8 . toUrlPiece

-- |
-- >>> toUrlPiece ()
-- "_"
instance ToHttpApiData () where
  toUrlPiece () = "_"
  toEncodedUrlPiece = unsafeToEncodedUrlPiece

instance ToHttpApiData Char where
  toUrlPiece = T.singleton

-- |
-- >>> toUrlPiece (Version [1, 2, 3] [])
-- "1.2.3"
instance ToHttpApiData Version where
  toUrlPiece = T.pack . showVersion
  toEncodedUrlPiece = unsafeToEncodedUrlPiece

#if MIN_VERSION_base(4,8,0)
instance ToHttpApiData Void    where toUrlPiece = absurd
instance ToHttpApiData Natural where toUrlPiece = showt; toEncodedUrlPiece = unsafeToEncodedUrlPiece
#endif

instance ToHttpApiData Bool     where toUrlPiece = showTextData; toEncodedUrlPiece = unsafeToEncodedUrlPiece
instance ToHttpApiData Ordering where toUrlPiece = showTextData; toEncodedUrlPiece = unsafeToEncodedUrlPiece

instance ToHttpApiData Double   where toUrlPiece = showt; toEncodedUrlPiece = unsafeToEncodedUrlPiece
instance ToHttpApiData Float    where toUrlPiece = showt; toEncodedUrlPiece = unsafeToEncodedUrlPiece
instance ToHttpApiData Int      where toUrlPiece = showt; toEncodedUrlPiece = unsafeToEncodedUrlPiece
instance ToHttpApiData Int8     where toUrlPiece = showt; toEncodedUrlPiece = unsafeToEncodedUrlPiece
instance ToHttpApiData Int16    where toUrlPiece = showt; toEncodedUrlPiece = unsafeToEncodedUrlPiece
instance ToHttpApiData Int32    where toUrlPiece = showt; toEncodedUrlPiece = unsafeToEncodedUrlPiece
instance ToHttpApiData Int64    where toUrlPiece = showt; toEncodedUrlPiece = unsafeToEncodedUrlPiece
instance ToHttpApiData Integer  where toUrlPiece = showt; toEncodedUrlPiece = unsafeToEncodedUrlPiece
instance ToHttpApiData Word     where toUrlPiece = showt; toEncodedUrlPiece = unsafeToEncodedUrlPiece
instance ToHttpApiData Word8    where toUrlPiece = showt; toEncodedUrlPiece = unsafeToEncodedUrlPiece
instance ToHttpApiData Word16   where toUrlPiece = showt; toEncodedUrlPiece = unsafeToEncodedUrlPiece
instance ToHttpApiData Word32   where toUrlPiece = showt; toEncodedUrlPiece = unsafeToEncodedUrlPiece
instance ToHttpApiData Word64   where toUrlPiece = showt; toEncodedUrlPiece = unsafeToEncodedUrlPiece

-- |
-- >>> toUrlPiece (fromGregorian 2015 10 03)
-- "2015-10-03"
instance ToHttpApiData Day where
  toUrlPiece = T.pack . show
  toEncodedUrlPiece = unsafeToEncodedUrlPiece

timeToUrlPiece :: FormatTime t => String -> t -> Text
timeToUrlPiece fmt = T.pack . formatTime defaultTimeLocale (iso8601DateFormat (Just fmt))

-- |
-- >>> toUrlPiece $ TimeOfDay 14 55 23.1
-- "14:55:23.1"
instance ToHttpApiData TimeOfDay where
  toUrlPiece = T.pack . formatTime defaultTimeLocale "%H:%M:%S%Q"
  toEncodedUrlPiece = unsafeToEncodedUrlPiece

-- |
-- >>> toUrlPiece $ LocalTime (fromGregorian 2015 10 03) (TimeOfDay 14 55 21.687)
-- "2015-10-03T14:55:21.687"
instance ToHttpApiData LocalTime where
  toUrlPiece = timeToUrlPiece "%H:%M:%S%Q"
  toEncodedUrlPiece = unsafeToEncodedUrlPiece

-- |
-- >>> toUrlPiece $ ZonedTime (LocalTime (fromGregorian 2015 10 03) (TimeOfDay 14 55 51.001)) utc
-- "2015-10-03T14:55:51.001+0000"
instance ToHttpApiData ZonedTime where
  toUrlPiece = timeToUrlPiece "%H:%M:%S%Q%z"
  toEncodedUrlPiece = unsafeToEncodedUrlPiece

-- |
-- >>> toUrlPiece $ UTCTime (fromGregorian 2015 10 03) 864.5
-- "2015-10-03T00:14:24.5Z"
instance ToHttpApiData UTCTime where
  toUrlPiece = timeToUrlPiece "%H:%M:%S%QZ"
  toEncodedUrlPiece = unsafeToEncodedUrlPiece

instance ToHttpApiData NominalDiffTime where
  toUrlPiece = toUrlPiece . (floor :: NominalDiffTime -> Integer)
  toEncodedUrlPiece = unsafeToEncodedUrlPiece

instance ToHttpApiData String   where toUrlPiece = T.pack
instance ToHttpApiData Text     where toUrlPiece = id
instance ToHttpApiData L.Text   where toUrlPiece = L.toStrict

instance ToHttpApiData All where toUrlPiece = toUrlPiece . getAll; toEncodedUrlPiece = toEncodedUrlPiece . getAll
instance ToHttpApiData Any where toUrlPiece = toUrlPiece . getAny; toEncodedUrlPiece = toEncodedUrlPiece . getAny

instance ToHttpApiData a => ToHttpApiData (Dual a) where
  toUrlPiece = toUrlPiece . getDual
  toEncodedUrlPiece = toEncodedUrlPiece . getDual

instance ToHttpApiData a => ToHttpApiData (Sum a) where
  toUrlPiece = toUrlPiece . getSum
  toEncodedUrlPiece = toEncodedUrlPiece . getSum

instance ToHttpApiData a => ToHttpApiData (Product a) where
  toUrlPiece = toUrlPiece . getProduct
  toEncodedUrlPiece = toEncodedUrlPiece . getProduct

instance ToHttpApiData a => ToHttpApiData (First a) where
  toUrlPiece = toUrlPiece . getFirst
  toEncodedUrlPiece = toEncodedUrlPiece . getFirst

instance ToHttpApiData a => ToHttpApiData (Last a) where
  toUrlPiece = toUrlPiece . getLast
  toEncodedUrlPiece = toEncodedUrlPiece . getLast

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
-- >>> let Right c = parseUrlPiece "SESSID=r2t5uvjq435r4q7ib3vtdjq120" :: Either Text SetCookie
-- >>> toUrlPiece c
-- "\"SESSID=r2t5uvjq435r4q7ib3vtdjq120\""
instance ToHttpApiData SetCookie where
  toUrlPiece = showt . BS.toLazyByteString . renderSetCookie
  toEncodedUrlPiece = renderSetCookie

-- |
-- >>> parseUrlPiece "_" :: Either Text ()
-- Right ()
instance FromHttpApiData () where
  parseUrlPiece "_" = pure ()
  parseUrlPiece s   = defaultParseError s

instance FromHttpApiData Char where
  parseUrlPiece s =
    case T.uncons s of
      Just (c, s') | T.null s' -> pure c
      _                        -> defaultParseError s

-- |
-- >>> showVersion <$> parseUrlPiece "1.2.3"
-- Right "1.2.3"
instance FromHttpApiData Version where
  parseUrlPiece s =
    case reverse (readP_to_S parseVersion (T.unpack s)) of
      ((x, ""):_) -> pure x
      _           -> defaultParseError s

#if MIN_VERSION_base(4,8,0)
-- | Parsing a @'Void'@ value is always an error, considering @'Void'@ as a data type with no constructors.
instance FromHttpApiData Void where
  parseUrlPiece _ = Left "Void cannot be parsed!"

instance FromHttpApiData Natural where
  parseUrlPiece s = do
    n <- runReader (signed decimal) s
    if n < 0
      then Left ("underflow: " <> s <> " (should be a non-negative integer)")
      else Right (fromInteger n)
#endif

instance FromHttpApiData Bool     where parseUrlPiece = parseBoundedUrlPiece
instance FromHttpApiData Ordering where parseUrlPiece = parseBoundedUrlPiece
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

-- |
-- >>> toGregorian <$> parseUrlPiece "2016-12-01"
-- Right (2016,12,1)
instance FromHttpApiData Day where parseUrlPiece = runAtto Atto.day

-- |
-- >>> parseUrlPiece "14:55:01.333" :: Either Text TimeOfDay
-- Right 14:55:01.333
instance FromHttpApiData TimeOfDay where parseUrlPiece = runAtto Atto.timeOfDay

-- |
-- >>> parseUrlPiece "2015-10-03T14:55:01" :: Either Text LocalTime
-- Right 2015-10-03 14:55:01
instance FromHttpApiData LocalTime where parseUrlPiece = runAtto Atto.localTime

-- |
-- >>> parseUrlPiece "2015-10-03T14:55:01+0000" :: Either Text ZonedTime
-- Right 2015-10-03 14:55:01 +0000
--
-- >>> parseQueryParam "2016-12-31T01:00:00Z" :: Either Text ZonedTime
-- Right 2016-12-31 01:00:00 +0000
instance FromHttpApiData ZonedTime where parseUrlPiece = runAtto Atto.zonedTime

-- |
-- >>> parseUrlPiece "2015-10-03T00:14:24Z" :: Either Text UTCTime
-- Right 2015-10-03 00:14:24 UTC
instance FromHttpApiData UTCTime   where parseUrlPiece = runAtto Atto.utcTime

instance FromHttpApiData NominalDiffTime where parseUrlPiece = fmap fromInteger . parseUrlPiece

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
    | T.toLower (T.take 7 s) == "nothing" = pure Nothing
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

instance ToHttpApiData UUID.UUID where
    toUrlPiece = UUID.toText
    toHeader   = UUID.toASCIIBytes
    toEncodedUrlPiece = unsafeToEncodedUrlPiece

instance FromHttpApiData UUID.UUID where
    parseUrlPiece = maybe (Left "invalid UUID") Right . UUID.fromText
    parseHeader   = maybe (Left "invalid UUID") Right . UUID.fromASCIIBytes


-- | Lenient parameters. 'FromHttpApiData' combinators always return `Right`.
--
-- @since 0.3.5
newtype LenientData a = LenientData { getLenientData :: Either Text a }
    deriving (Eq, Ord, Show, Read, Typeable, Data, Functor, Foldable, Traversable)

instance FromHttpApiData a => FromHttpApiData (LenientData a) where
    parseUrlPiece   = Right . LenientData . parseUrlPiece
    parseHeader     = Right . LenientData . parseHeader
    parseQueryParam = Right . LenientData . parseQueryParam

-- |
-- >>> parseUrlPiece "SESSID=r2t5uvjq435r4q7ib3vtdjq120" :: Either Text SetCookie
-- Right (SetCookie {setCookieName = "SESSID", setCookieValue = "r2t5uvjq435r4q7ib3vtdjq120", setCookiePath = Nothing, setCookieExpires = Nothing, setCookieMaxAge = Nothing, setCookieDomain = Nothing, setCookieHttpOnly = False, setCookieSecure = False, setCookieSameSite = Nothing})
instance FromHttpApiData SetCookie where
  parseUrlPiece = parseHeader  . encodeUtf8
  parseHeader   = Right . parseSetCookie
-------------------------------------------------------------------------------
-- Attoparsec helpers
-------------------------------------------------------------------------------

runAtto :: Atto.Parser a -> Text -> Either Text a
runAtto p t = case Atto.parseOnly (p <* Atto.endOfInput) t of
    Left err -> Left (T.pack err)
    Right x  -> Right x
