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
#endif

import           Control.Monad              ((<=<))
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import           Data.Int
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

import           GHC.Exts                   (IsList (..))
import           GHC.Generics
import           Network.URI                (escapeURIString, isUnreserved,
                                             unEscapeString)

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

-- | The contents of a form, not yet URL-encoded.
--
-- 'Form' can be URL-encoded with 'encodeForm' and URL-decoded with 'decodeForm'.
newtype Form = Form { unForm :: Map Text [Text] }
  deriving (Eq, Read, Generic, Monoid)

instance Show Form where
  showsPrec d form = showParen (d > 10) $
    showString "fromList " . shows (toList form)

instance IsList Form where
  type Item Form = (Text, Text)
  fromList = Form . Map.fromListWith (flip (<>)) . fmap (\(k, v) -> (k, [v]))
  toList = concatMap (\(k, vs) -> map ((,) k) vs) . Map.toList . unForm

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

instance ToForm [(Text, Text)] where toForm = fromList
instance ToForm (Map Text [Text]) where toForm = Form . Map.filter (not . null)
instance ToForm Form where toForm = id

-- | A 'Generic'-based implementation of 'toForm'.
-- This is used as a default implementation in 'ToForm'.
genericToForm :: (Generic a, GToForm (Rep a)) => a -> Form
genericToForm = gToForm . from

class GToForm (f :: * -> *) where
  gToForm :: f x -> Form

instance (GToForm f, GToForm g) => GToForm (f :*: g) where
  gToForm (a :*: b) = gToForm a <> gToForm b

instance (GToForm f, GToForm g) => GToForm (f :+: g) where
  gToForm (L1 a) = gToForm a
  gToForm (R1 a) = gToForm a

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
--     '\<$\>' maybe (Left "key \"name\" not found") 'parseQueryParam' (lookup "name" m)
--     '\<*\>' maybe (Left "key \"age\" not found")  'parseQueryParam' (lookup "name" m)
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

instance FromForm Form where fromForm = return
instance FromForm (Map Text [Text]) where fromForm = return . unForm
instance FromForm [(Text, Text)] where fromForm = return . toList

-- | A 'Generic'-based implementation of 'fromForm'.
-- This is used as a default implementation in 'FromForm'.
genericFromForm :: (Generic a, GFromForm (Rep a))
    => Form -> Either Text a
genericFromForm f = to <$> gFromForm f

class GFromForm (f :: * -> *) where
  gFromForm :: Form -> Either Text (f x)

instance (GFromForm f, GFromForm g) => GFromForm (f :*: g) where
  gFromForm f = (:*:) <$> gFromForm f <*> gFromForm f

instance (GFromForm f, GFromForm g) => GFromForm (f :+: g) where
  gFromForm f
      = fmap L1 (gFromForm f)
    <!> fmap R1 (gFromForm f)
    where
      Left _  <!> y = y
      x       <!> _ = x

instance (Selector s, FromHttpApiData f) => GFromForm (M1 S s (K1 i f)) where
  gFromForm f =
    case concat (Map.lookup key (unForm f)) of
      [v] -> M1 . K1 <$> parseQueryParam v
      []  -> Left $ "Could not find key " <> Text.pack (show key)
      _   -> Left $ "Duplicate key " <> Text.pack (show key)
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
-- >>> encodeForm [("name", "Julian"), ("lastname", "Arni")]
-- "lastname=Arni&name=Julian"
--
-- Keys with empty values get encoded to just @key@ (without the @=@ sign):
--
-- >>> encodeForm [("is_test", "")]
-- "is_test"
--
-- Empty keys are allowed too:
--
-- >>> encodeForm [("", "foobar")]
-- "=foobar"
--
-- However, if not key and value are empty, the key-value pair is ignored.
-- (This prevents @'decodeForm' . 'encodeForm'@ from being a true isomorphism).
--
-- >>> encodeForm [("", "")]
-- ""
--
-- Everything is escaped with @'escapeURIString' 'isUnreserved'@:
--
-- >>> encodeForm [("fullname", "Andres Löh")]
-- "fullname=Andres%20L%C3%B6h"
encodeForm :: Form -> BSL.ByteString
encodeForm xs = BSL.intercalate "&" $ map (BSL.fromStrict . encodePair) $ toList xs
  where
    escape = Text.encodeUtf8 . Text.pack . escapeURIString isUnreserved . Text.unpack

    encodePair (k, "") = escape k
    encodePair (k, v) = escape k <> "=" <> escape v


-- | Decode an @application/x-www-form-urlencoded@ 'BSL.ByteString' to a 'Form'.
--
-- Key-value pairs get decoded normally:
--
-- >>> decodeForm "name=Greg&lastname=Weber"
-- Right (fromList [("lastname","Weber"),("name","Greg")])
--
-- Keys with no values get decoded to pairs with empty values.
--
-- >>> decodeForm "is_test"
-- Right (fromList [("is_test","")])
--
-- Empty keys are allowed:
--
-- >>> decodeForm "=foobar"
-- Right (fromList [("","foobar")])
--
-- The empty string gets decoded into an empty 'Form':
--
-- >>> decodeForm ""
-- Right (fromList [])
--
-- Everything is un-escaped with 'unEscapeString':
--
-- >>> decodeForm "fullname=Andres%20L%C3%B6h"
-- Right (fromList [("fullname","Andres L\246h")])
--
-- Improperly formed strings result in an error:
--
-- >>> decodeForm "this=has=too=many=equals"
-- Left "not a valid pair: this=has=too=many=equals"
decodeForm :: BSL.ByteString -> Either Text Form
decodeForm bs = toForm <$> traverse parsePair pairs
  where
    pairs = map (Text.decodeUtf8With lenientDecode . BSL.toStrict) (BSL8.split '&' bs)

    unescape = Text.pack . unEscapeString . Text.unpack . Text.replace "+" "%20"

    parsePair :: Text -> Either Text (Text, Text)
    parsePair p =
      case Text.splitOn "=" p of
        [k, v] -> return (unescape k, unescape v)
        [k]    -> return (unescape k, "" )
        _ -> Left $ "not a valid pair: " <> p

data Proxy3 a b c = Proxy3

-- | This is a convenience function for decoding a
-- @application/x-www-form-urlencoded@ 'BSL.ByteString' directly to a datatype
-- that has an instance of 'FromForm'.
--
-- This is effectively @'fromForm' '<=<' 'decodeForm'@.
--
-- >>> decodeAsForm "name=Dennis&age=22" :: Either Text Person
-- Right (Person {name = "Dennis", age = 22})
decodeAsForm :: FromForm a => BSL.ByteString -> Either Text a
decodeAsForm = fromForm <=< decodeForm

-- | This is a convenience function for encoding a datatype that has instance
-- of 'ToForm' directly to a @application/x-www-form-urlencoded@
-- 'BSL.ByteString'.
--
-- This is effectively @'encodeForm' . 'toForm'@.
--
-- >>> encodeAsForm Person {name = "Dennis", age = 22}
-- "age=22&name=Dennis"
encodeAsForm :: ToForm a => a -> BSL.ByteString
encodeAsForm = encodeForm . toForm
