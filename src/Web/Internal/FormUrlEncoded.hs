{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
module Web.Internal.FormUrlEncoded where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
import Data.Traversable
#endif

import           Control.Arrow              ((***))
import           Control.Monad              ((<=<))
import           Data.ByteString.Builder    (toLazyByteString, shortByteString)
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Foldable              as F
import           Data.Hashable              (Hashable)
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Data.Int
import           Data.IntMap                (IntMap)
import qualified Data.IntMap                as IntMap
import           Data.List                  (intersperse)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Monoid

import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Lazy             as Lazy
import           Data.Text.Encoding         as Text
import           Data.Text.Encoding.Error   (lenientDecode)

import           Data.Time
import           Data.Word

#if MIN_VERSION_base(4,8,0)
import Data.Void
import Numeric.Natural
#endif

import           GHC.Exts                   (IsList (..))
import           GHC.Generics
import           URI.ByteString             (urlEncodeQuery, urlDecodeQuery)

import Web.Internal.HttpApiData

-- $setup
-- >>> :set -XDeriveGeneric
-- >>> :set -XOverloadedLists
-- >>> :set -XOverloadedStrings
-- >>> :set -XFlexibleContexts
-- >>> :set -XScopedTypeVariables
-- >>> :set -XTypeFamilies
-- >>> import Data.Either (isLeft)
-- >>> import Data.List (sort)
-- >>> data Person = Person { name :: String, age :: Int } deriving (Show, Generic)
-- >>> instance ToForm Person
-- >>> instance FromForm Person

-- | Typeclass for types that can be used as keys in a 'Form'-like container (like 'Map').
class ToFormKey k where
  -- | Render a key for a 'Form'.
  toFormKey :: k -> Text

instance ToFormKey ()       where toFormKey = toQueryParam
instance ToFormKey Char     where toFormKey = toQueryParam

instance ToFormKey Bool     where toFormKey = toQueryParam
instance ToFormKey Ordering where toFormKey = toQueryParam

instance ToFormKey Double   where toFormKey = toQueryParam
instance ToFormKey Float    where toFormKey = toQueryParam
instance ToFormKey Int      where toFormKey = toQueryParam
instance ToFormKey Int8     where toFormKey = toQueryParam
instance ToFormKey Int16    where toFormKey = toQueryParam
instance ToFormKey Int32    where toFormKey = toQueryParam
instance ToFormKey Int64    where toFormKey = toQueryParam
instance ToFormKey Integer  where toFormKey = toQueryParam
instance ToFormKey Word     where toFormKey = toQueryParam
instance ToFormKey Word8    where toFormKey = toQueryParam
instance ToFormKey Word16   where toFormKey = toQueryParam
instance ToFormKey Word32   where toFormKey = toQueryParam
instance ToFormKey Word64   where toFormKey = toQueryParam

instance ToFormKey Day              where toFormKey = toQueryParam
instance ToFormKey LocalTime        where toFormKey = toQueryParam
instance ToFormKey ZonedTime        where toFormKey = toQueryParam
instance ToFormKey UTCTime          where toFormKey = toQueryParam
instance ToFormKey NominalDiffTime  where toFormKey = toQueryParam

instance ToFormKey String     where toFormKey = toQueryParam
instance ToFormKey Text       where toFormKey = toQueryParam
instance ToFormKey Lazy.Text  where toFormKey = toQueryParam

instance ToFormKey All where toFormKey = toQueryParam
instance ToFormKey Any where toFormKey = toQueryParam

instance ToFormKey a => ToFormKey (Dual a)    where toFormKey = toFormKey . getDual
instance ToFormKey a => ToFormKey (Sum a)     where toFormKey = toFormKey . getSum
instance ToFormKey a => ToFormKey (Product a) where toFormKey = toFormKey . getProduct

#if MIN_VERSION_base(4,8,0)
instance ToFormKey Void     where toFormKey = toQueryParam
instance ToFormKey Natural  where toFormKey = toQueryParam
#endif

-- | Typeclass for types that can be parsed from keys of a 'Form'. This is the reverse of 'ToFormKey'.
class FromFormKey k where
  -- | Parse a key of a 'Form'.
  parseFormKey :: Text -> Either Text k

instance FromFormKey ()       where parseFormKey = parseQueryParam
instance FromFormKey Char     where parseFormKey = parseQueryParam

instance FromFormKey Bool     where parseFormKey = parseQueryParam
instance FromFormKey Ordering where parseFormKey = parseQueryParam

instance FromFormKey Double   where parseFormKey = parseQueryParam
instance FromFormKey Float    where parseFormKey = parseQueryParam
instance FromFormKey Int      where parseFormKey = parseQueryParam
instance FromFormKey Int8     where parseFormKey = parseQueryParam
instance FromFormKey Int16    where parseFormKey = parseQueryParam
instance FromFormKey Int32    where parseFormKey = parseQueryParam
instance FromFormKey Int64    where parseFormKey = parseQueryParam
instance FromFormKey Integer  where parseFormKey = parseQueryParam
instance FromFormKey Word     where parseFormKey = parseQueryParam
instance FromFormKey Word8    where parseFormKey = parseQueryParam
instance FromFormKey Word16   where parseFormKey = parseQueryParam
instance FromFormKey Word32   where parseFormKey = parseQueryParam
instance FromFormKey Word64   where parseFormKey = parseQueryParam

instance FromFormKey Day              where parseFormKey = parseQueryParam
instance FromFormKey LocalTime        where parseFormKey = parseQueryParam
instance FromFormKey ZonedTime        where parseFormKey = parseQueryParam
instance FromFormKey UTCTime          where parseFormKey = parseQueryParam
instance FromFormKey NominalDiffTime  where parseFormKey = parseQueryParam

instance FromFormKey String     where parseFormKey = parseQueryParam
instance FromFormKey Text       where parseFormKey = parseQueryParam
instance FromFormKey Lazy.Text  where parseFormKey = parseQueryParam

instance FromFormKey All where parseFormKey = parseQueryParam
instance FromFormKey Any where parseFormKey = parseQueryParam

instance FromFormKey a => FromFormKey (Dual a)    where parseFormKey = fmap Dual . parseFormKey
instance FromFormKey a => FromFormKey (Sum a)     where parseFormKey = fmap Sum . parseFormKey
instance FromFormKey a => FromFormKey (Product a) where parseFormKey = fmap Product . parseFormKey

#if MIN_VERSION_base(4,8,0)
instance FromFormKey Void     where parseFormKey = parseQueryParam
instance FromFormKey Natural  where parseFormKey = parseQueryParam
#endif

-- | The contents of a form, not yet URL-encoded.
--
-- 'Form' can be URL-encoded with 'urlEncodeForm' and URL-decoded with 'urlDecodeForm'.
newtype Form = Form { unForm :: HashMap Text [Text] }
  deriving (Eq, Read, Generic, Monoid)

instance Show Form where
  showsPrec d form = showParen (d > 10) $
    showString "fromList " . shows (toList form)

instance IsList Form where
  type Item Form = (Text, Text)
  fromList = Form . HashMap.fromListWith (flip (<>)) . fmap (\(k, v) -> (k, [v]))
  toList = concatMap (\(k, vs) -> map ((,) k) vs) . HashMap.toList . unForm

-- | Convert a value into 'Form'.
--
-- An example type and instance:
--
-- @
-- {-\# LANGUAGE OverloadedLists \#-}
--
-- data Person = Person
--   { name :: String
--   , age  :: Int }
--
-- instance 'ToForm' Person where
--   'toForm' person =
--     [ (\"name\", 'toQueryParam' (name person))
--     , (\"age\", 'toQueryParam' (age person)) ]
-- @
--
-- Instead of manually writing @'ToForm'@ instances you can
-- use a default generic implementation of @'toForm'@.
--
-- To do that, simply add @deriving 'Generic'@ clause to your datatype
-- and declare a 'ToForm' instance for your datatype without
-- giving definition for 'toForm'.
--
-- For instance, the previous example can be simplified into this:
--
-- @
-- data Person = Person
--   { name :: String
--   , age  :: Int
--   } deriving ('Generic')
--
-- instance 'ToForm' Person
-- @
--
-- The default implementation will use 'toQueryParam' for each field's value.
class ToForm a where
  -- | Convert a value into 'Form'.
  toForm :: a -> Form
  default toForm :: (Generic a, GToForm (Rep a)) => a -> Form
  toForm = genericToForm

instance ToForm Form where toForm = id

instance (ToFormKey k, ToHttpApiData v) => ToForm [(k, v)] where
  toForm = fromList . map (toFormKey *** toQueryParam)

instance (ToFormKey k, ToHttpApiData v) => ToForm (Map k [v]) where
  toForm = fromEntriesByKey . Map.toList

instance (ToFormKey k, ToHttpApiData v) => ToForm (HashMap k [v]) where
  toForm = fromEntriesByKey . HashMap.toList

instance ToHttpApiData v => ToForm (IntMap [v]) where
  toForm = fromEntriesByKey . IntMap.toList

-- | Convert a list of entries groupped by key into a 'Form'.
--
-- >>> fromEntriesByKey [("name",["Nick"]),("color",["red","blue"])]
-- fromList [("color","red"),("color","blue"),("name","Nick")]
fromEntriesByKey :: (ToFormKey k, ToHttpApiData v) => [(k, [v])] -> Form
fromEntriesByKey = Form . HashMap.fromListWith (<>) . map (toFormKey *** map toQueryParam)

data Proxy3 a b c = Proxy3

-- | A 'Generic'-based implementation of 'toForm'.
-- This is used as a default implementation in 'ToForm'.
genericToForm :: (Generic a, GToForm (Rep a)) => a -> Form
genericToForm = gToForm . from

class GToForm (f :: * -> *) where
  gToForm :: f x -> Form

instance (GToForm f, GToForm g) => GToForm (f :*: g) where
  gToForm (a :*: b) = gToForm a <> gToForm b

instance (GToForm f) => GToForm (M1 D x f) where
  gToForm (M1 a) = gToForm a

instance (GToForm f) => GToForm (M1 C x f) where
  gToForm (M1 a) = gToForm a

instance (Selector s, ToHttpApiData c) => GToForm (M1 S s (K1 i c)) where
  gToForm (M1 (K1 c)) = fromList [(key, toQueryParam c)]
    where
      key = Text.pack $ selName (Proxy3 :: Proxy3 s g p)

-- | Parse 'Form' into a value.
--
-- An example type and instance:
--
-- @
-- data Person = Person
--   { name :: String
--   , age  :: Int }
--
-- instance 'FromForm' Person where
--   'fromForm' (Form m) = Person
--     '<$>' 'parseUnique' "name" m
--     '<*>' 'parseUnique' "age"  m
-- @
--
-- Instead of manually writing @'FromForm'@ instances you can
-- use a default generic implementation of @'fromForm'@.
--
-- To do that, simply add @deriving 'Generic'@ clause to your datatype
-- and declare a 'FromForm' instance for your datatype without
-- giving definition for 'fromForm'.
--
-- For instance, the previous example can be simplified into this:
--
-- @
-- data Person = Person
--   { name :: String
--   , age  :: Int
--   } deriving ('Generic')
--
-- instance 'FromForm' Person
-- @
--
-- The default implementation will use 'parseQueryParam' for each field's value.
class FromForm a where
  -- | Parse 'Form' into a value.
  fromForm :: Form -> Either Text a
  default fromForm :: (Generic a, GFromForm (Rep a))
     => Form -> Either Text a
  fromForm = genericFromForm

instance FromForm Form where fromForm = pure

instance (FromFormKey k, FromHttpApiData v) => FromForm [(k, v)] where
  fromForm = fmap (concatMap (\(k, vs) -> map ((,) k) vs)) . toEntriesByKey

instance (Ord k, FromFormKey k, FromHttpApiData v) => FromForm (Map k [v]) where
  fromForm = fmap (Map.fromListWith (<>)) . toEntriesByKey

instance (Eq k, Hashable k, FromFormKey k, FromHttpApiData v) => FromForm (HashMap k [v]) where
  fromForm = fmap (HashMap.fromListWith (<>)) . toEntriesByKey

instance FromHttpApiData v => FromForm (IntMap [v]) where
  fromForm = fmap (IntMap.fromListWith (<>)) . toEntriesByKey

-- | Parse a 'Form' into a list of entries groupped by key.
--
-- >>> toEntriesByKey [("name", "Nick"), ("color", "red"), ("color", "white")] :: Either Text [(Text, [Text])]
-- Right [("color",["red","white"]),("name",["Nick"])]
toEntriesByKey :: (FromFormKey k, FromHttpApiData v) => Form -> Either Text [(k, [v])]
toEntriesByKey = traverse parseGroup . HashMap.toList . unForm
  where
    parseGroup (k, vs) = (,) <$> parseFormKey k <*> traverse parseQueryParam vs

-- | A 'Generic'-based implementation of 'fromForm'.
-- This is used as a default implementation in 'FromForm'.
genericFromForm :: (Generic a, GFromForm (Rep a))
    => Form -> Either Text a
genericFromForm f = to <$> gFromForm f

class GFromForm (f :: * -> *) where
  gFromForm :: Form -> Either Text (f x)

instance (GFromForm f, GFromForm g) => GFromForm (f :*: g) where
  gFromForm f = (:*:) <$> gFromForm f <*> gFromForm f

instance (Selector s, FromHttpApiData f) => GFromForm (M1 S s (K1 i f)) where
  gFromForm form = M1 . K1 <$> parseUnique key form
    where
      key = Text.pack $ selName (Proxy3 :: Proxy3 s g p)

instance (GFromForm f) => GFromForm (M1 D x f) where
  gFromForm f = M1 <$> gFromForm f

instance (GFromForm f) => GFromForm (M1 C x f) where
  gFromForm f = M1 <$> gFromForm f

-- | Encode a 'Form' to an @application/x-www-form-urlencoded@ 'BSL.ByteString'.
--
-- Key-value pairs get encoded to @key=value@ and separated by @&@:
--
-- >>> urlEncodeForm [("name", "Julian"), ("lastname", "Arni")]
-- "lastname=Arni&name=Julian"
--
-- Keys with empty values get encoded to just @key@ (without the @=@ sign):
--
-- >>> urlEncodeForm [("is_test", "")]
-- "is_test"
--
-- Empty keys are allowed too:
--
-- >>> urlEncodeForm [("", "foobar")]
-- "=foobar"
--
-- However, if not key and value are empty, the key-value pair is ignored.
-- (This prevents @'urlDecodeForm' . 'urlEncodeForm'@ from being a true isomorphism).
--
-- >>> urlEncodeForm [("", "")]
-- ""
--
-- Everything is escaped with @'escapeURIString' 'isUnreserved'@:
--
-- >>> urlEncodeForm [("fullname", "Andres Löh")]
-- "fullname=Andres%20L%C3%B6h"
urlEncodeForm :: Form -> BSL.ByteString
urlEncodeForm = toLazyByteString . mconcat . intersperse (shortByteString "&") . map encodePair . toList
  where
    escape = urlEncodeQuery . Text.encodeUtf8

    encodePair (k, "") = escape k
    encodePair (k, v) = escape k <> shortByteString "=" <> escape v


-- | Decode an @application/x-www-form-urlencoded@ 'BSL.ByteString' to a 'Form'.
--
-- Key-value pairs get decoded normally:
--
-- >>> urlDecodeForm "name=Greg&lastname=Weber"
-- Right (fromList [("lastname","Weber"),("name","Greg")])
--
-- Keys with no values get decoded to pairs with empty values.
--
-- >>> urlDecodeForm "is_test"
-- Right (fromList [("is_test","")])
--
-- Empty keys are allowed:
--
-- >>> urlDecodeForm "=foobar"
-- Right (fromList [("","foobar")])
--
-- The empty string gets decoded into an empty 'Form':
--
-- >>> urlDecodeForm ""
-- Right (fromList [])
--
-- Everything is un-escaped with 'unEscapeString':
--
-- >>> urlDecodeForm "fullname=Andres%20L%C3%B6h"
-- Right (fromList [("fullname","Andres L\246h")])
--
-- Improperly formed strings result in an error:
--
-- >>> urlDecodeForm "this=has=too=many=equals"
-- Left "not a valid pair: this=has=too=many=equals"
urlDecodeForm :: BSL.ByteString -> Either Text Form
urlDecodeForm bs = toForm <$> traverse parsePair pairs
  where
    pairs = map (BSL8.split '=') (BSL8.split '&' bs)

    unescape = Text.decodeUtf8With lenientDecode . urlDecodeQuery . BSL.toStrict

    parsePair p =
      case map unescape p of
        [k, v] -> return (k, v)
        [k]    -> return (k, "")
        xs     -> Left $ "not a valid pair: " <> Text.intercalate "=" xs

-- | This is a convenience function for decoding a
-- @application/x-www-form-urlencoded@ 'BSL.ByteString' directly to a datatype
-- that has an instance of 'FromForm'.
--
-- This is effectively @'fromForm' '<=<' 'urlDecodeForm'@.
--
-- >>> urlDecodeAsForm "name=Dennis&age=22" :: Either Text Person
-- Right (Person {name = "Dennis", age = 22})
urlDecodeAsForm :: FromForm a => BSL.ByteString -> Either Text a
urlDecodeAsForm = fromForm <=< urlDecodeForm

-- | This is a convenience function for encoding a datatype that has instance
-- of 'ToForm' directly to a @application/x-www-form-urlencoded@
-- 'BSL.ByteString'.
--
-- This is effectively @'urlEncodeForm' . 'toForm'@.
--
-- >>> urlEncodeAsForm Person {name = "Dennis", age = 22}
-- "age=22&name=Dennis"
urlEncodeAsForm :: ToForm a => a -> BSL.ByteString
urlEncodeAsForm = urlEncodeForm . toForm

-- | Find all values corresponding to a given key in a 'Form'.
--
-- >>> lookupAll "name" []
-- []
-- >>> lookupAll "name" [("name", "Oleg")]
-- ["Oleg"]
-- >>> lookupAll "name" [("name", "Oleg"), ("name", "David")]
-- ["Oleg","David"]
lookupAll :: Text -> Form -> [Text]
lookupAll key = F.concat . HashMap.lookup key . unForm

-- | Lookup a unique value for a key.
-- Fail if there is zero or more than one value.
--
-- >>> lookupUnique "name" []
-- Left "Could not find key \"name\""
-- >>> lookupUnique "name" [("name", "Oleg")]
-- Right "Oleg"
-- >>> lookupUnique "name" [("name", "Oleg"), ("name", "David")]
-- Left "Duplicate key \"name\""
lookupUnique :: Text -> Form -> Either Text Text
lookupUnique key form =
  case lookupAll key form of
    [v] -> pure v
    []  -> Left $ "Could not find key " <> Text.pack (show key)
    _   -> Left $ "Duplicate key " <> Text.pack (show key)

-- | Lookup all values for a given key in a 'Form' and parse them with 'parseQueryParams'.
--
-- >>> parseAll "age" [] :: Either Text [Word8]
-- Right []
-- >>> parseAll "age" [("age", "8"), ("age", "seven")] :: Either Text [Word8]
-- Left "could not parse: `seven' (input does not start with a digit)"
-- >>> parseAll "age" [("age", "8"), ("age", "777")] :: Either Text [Word8]
-- Left "out of bounds: `777' (should be between 0 and 255)"
-- >>> parseAll "age" [("age", "12"), ("age", "25")] :: Either Text [Word8]
-- Right [12,25]
parseAll :: FromHttpApiData v => Text -> Form -> Either Text [v]
parseAll key = parseQueryParams . lookupAll key

-- | Lookup a unique value for a given key and parse it with 'parseQueryParam'.
-- Fail if there is zero or more than one value for the key.
--
-- >>> parseUnique "age" [] :: Either Text Word8
-- Left "Could not find key \"age\""
-- >>> parseUnique "age" [("age", "12"), ("age", "25")] :: Either Text Word8
-- Left "Duplicate key \"age\""
-- >>> parseUnique "age" [("age", "seven")] :: Either Text Word8
-- Left "could not parse: `seven' (input does not start with a digit)"
-- >>> parseUnique "age" [("age", "777")] :: Either Text Word8
-- Left "out of bounds: `777' (should be between 0 and 255)"
-- >>> parseUnique "age" [("age", "7")] :: Either Text Word8
-- Right 7
parseUnique :: FromHttpApiData v => Text -> Form -> Either Text v
parseUnique key form = lookupUnique key form >>= parseQueryParam

