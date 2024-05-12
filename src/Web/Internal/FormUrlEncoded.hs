{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module Web.Internal.FormUrlEncoded where

import           Control.Applicative        (Const(Const))
import           Control.Arrow              ((***))
import           Control.Monad              ((<=<))
import           Data.ByteString.Builder    (shortByteString, toLazyByteString)
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import           Data.Coerce                (coerce)
import qualified Data.Foldable              as F
import           Data.Functor.Identity      (Identity(Identity))
import           Data.Hashable              (Hashable)
import qualified Data.Map.Strict            as Map
import           Data.Int                   (Int16, Int32, Int64, Int8)
import           Data.IntMap                (IntMap)
import qualified Data.IntMap                as IntMap
import           Data.List                  (intersperse, sortBy)
import           Data.Monoid                (All (..), Any (..), Dual (..),
                                             Product (..), Sum (..))
import           Data.Ord                   (comparing)
import           Data.Proxy                 (Proxy (..))
import           Data.Semigroup             (Semigroup (..))
import qualified Data.Semigroup             as Semi
import           Data.Tagged                (Tagged (..))
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import           Data.Text.Encoding.Error   (lenientDecode)
import qualified Data.Text.Lazy             as Lazy
import           Data.Time.Compat           (Day, LocalTime, NominalDiffTime,
                                             UTCTime, ZonedTime)
import           Data.Time.Calendar.Month.Compat (Month)
import           Data.Time.Calendar.Quarter.Compat (Quarter, QuarterOfYear (..))
import           Data.Void                  (Void)
import           Data.Word                  (Word16, Word32, Word64, Word8)
import           GHC.Exts                   (Constraint, IsList (..))
import           GHC.Generics
import           GHC.TypeLits
import           Network.HTTP.Types.URI     (urlDecode, urlEncodeBuilder)
import           Numeric.Natural            (Natural)
import           Web.Internal.HttpApiData

-- $setup
-- >>> :set -XDeriveGeneric -XOverloadedLists -XOverloadedStrings -XFlexibleContexts -XScopedTypeVariables -XTypeFamilies
-- >>> import GHC.Generics (Generic)
-- >>> import Data.Char (toLower)
-- >>> import Data.Text (Text)
-- >>> import Data.Word (Word8)
--
-- >>> data Person = Person { name :: String, age :: Int } deriving (Show, Generic)
-- >>> instance ToForm Person
-- >>> instance FromForm Person
--
-- >>> data Post = Post { title :: String, subtitle :: Maybe String, comments :: [String]} deriving (Generic, Show)
-- >>> instance ToForm Post
-- >>> instance FromForm Post
--
-- >>> data Project = Project { projectName :: String, projectSize :: Int } deriving (Generic, Show)
-- >>> let myOptions = FormOptions { fieldLabelModifier = map toLower . drop (length ("project" :: String)) }
-- >>> instance ToForm Project where toForm = genericToForm myOptions
-- >>> instance FromForm Project where fromForm = genericFromForm myOptions

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
instance ToFormKey Quarter          where toFormKey = toQueryParam
instance ToFormKey QuarterOfYear    where toFormKey = toQueryParam
instance ToFormKey Month            where toFormKey = toQueryParam

instance ToFormKey String     where toFormKey = toQueryParam
instance ToFormKey Text       where toFormKey = toQueryParam
instance ToFormKey Lazy.Text  where toFormKey = toQueryParam

instance ToFormKey All where toFormKey = toQueryParam
instance ToFormKey Any where toFormKey = toQueryParam

instance ToFormKey a => ToFormKey (Dual a)    where toFormKey = coerce (toFormKey :: a -> Text)
instance ToFormKey a => ToFormKey (Sum a)     where toFormKey = coerce (toFormKey :: a -> Text)
instance ToFormKey a => ToFormKey (Product a) where toFormKey = coerce (toFormKey :: a -> Text)

instance ToFormKey a => ToFormKey (Semi.Min a)   where toFormKey = coerce (toFormKey :: a -> Text)
instance ToFormKey a => ToFormKey (Semi.Max a)   where toFormKey = coerce (toFormKey :: a -> Text)
instance ToFormKey a => ToFormKey (Semi.First a) where toFormKey = coerce (toFormKey :: a -> Text)
instance ToFormKey a => ToFormKey (Semi.Last a)  where toFormKey = coerce (toFormKey :: a -> Text)

instance ToFormKey a => ToFormKey (Tagged b a)  where toFormKey = coerce (toFormKey :: a -> Text)

-- | @since 0.4.2
instance ToFormKey a => ToFormKey (Identity a)   where toFormKey = coerce (toFormKey :: a -> Text)

-- | @since 0.4.2
instance ToFormKey a => ToFormKey (Const a b) where
    toFormKey = coerce (toFormKey :: a -> Text)

instance ToFormKey Void     where toFormKey = toQueryParam
instance ToFormKey Natural  where toFormKey = toQueryParam

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
instance FromFormKey Quarter          where parseFormKey = parseQueryParam
instance FromFormKey QuarterOfYear    where parseFormKey = parseQueryParam
instance FromFormKey Month            where parseFormKey = parseQueryParam

instance FromFormKey String     where parseFormKey = parseQueryParam
instance FromFormKey Text       where parseFormKey = parseQueryParam
instance FromFormKey Lazy.Text  where parseFormKey = parseQueryParam

instance FromFormKey All where parseFormKey = parseQueryParam
instance FromFormKey Any where parseFormKey = parseQueryParam

instance FromFormKey a => FromFormKey (Dual a)    where parseFormKey = coerce (parseFormKey :: Text -> Either Text a)
instance FromFormKey a => FromFormKey (Sum a)     where parseFormKey = coerce (parseFormKey :: Text -> Either Text a)
instance FromFormKey a => FromFormKey (Product a) where parseFormKey = coerce (parseFormKey :: Text -> Either Text a)

instance FromFormKey a => FromFormKey (Semi.Min a)   where parseFormKey = coerce (parseFormKey :: Text -> Either Text a)
instance FromFormKey a => FromFormKey (Semi.Max a)   where parseFormKey = coerce (parseFormKey :: Text -> Either Text a)
instance FromFormKey a => FromFormKey (Semi.First a) where parseFormKey = coerce (parseFormKey :: Text -> Either Text a)
instance FromFormKey a => FromFormKey (Semi.Last a)  where parseFormKey = coerce (parseFormKey :: Text -> Either Text a)

instance FromFormKey a => FromFormKey (Tagged b a) where parseFormKey = coerce (parseFormKey :: Text -> Either Text a)

-- | @since 0.4.2
instance FromFormKey a => FromFormKey (Identity a) where parseFormKey = coerce (parseFormKey :: Text -> Either Text a)

-- | @since 0.4.2
instance FromFormKey a => FromFormKey (Const a b) where
    parseFormKey = coerce (parseFormKey :: Text -> Either Text a)

instance FromFormKey Void     where parseFormKey = parseQueryParam
instance FromFormKey Natural  where parseFormKey = parseQueryParam

-- | The contents of a form, not yet URL-encoded.
--
-- 'Form' can be URL-encoded with 'urlEncodeForm' and URL-decoded with 'urlDecodeForm'.
newtype Form = Form { unForm :: Map.Map Text [Text] }
  deriving (Eq, Read, Generic, Semigroup, Monoid)

instance Show Form where
  showsPrec d form = showParen (d > 10) $
    showString "fromList " . shows (toListStable form)

-- | _NOTE:_ 'toList' is unstable and may result in different key order (but not values).
-- For a stable conversion use 'toListStable'.
instance IsList Form where
  type Item Form = (Text, Text)
  fromList = Form . Map.fromListWith (flip (<>)) . fmap (\(k, v) -> (k, [v]))
  toList = concatMap (\(k, vs) -> map ((,) k) vs) . Map.toList . unForm

-- | A stable version of 'toList'.
toListStable :: Form -> [(Text, Text)]
toListStable = sortOn fst . toList

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
-- The default implementation of 'toForm' is 'genericToForm'.
class ToForm a where
  -- | Convert a value into 'Form'.
  toForm :: a -> Form
  default toForm :: (Generic a, GToForm a (Rep a)) => a -> Form
  toForm = genericToForm defaultFormOptions

instance ToForm Form where toForm = id

instance (ToFormKey k, ToHttpApiData v) => ToForm [(k, v)] where
  toForm = fromList . map (toFormKey *** toQueryParam)

instance (ToFormKey k, ToHttpApiData v) => ToForm (Map.Map k [v]) where
  toForm = fromEntriesByKey . Map.toList

instance ToHttpApiData v => ToForm (IntMap [v]) where
  toForm = fromEntriesByKey . IntMap.toList

-- | Convert a list of entries groupped by key into a 'Form'.
--
-- >>> fromEntriesByKey [("name",["Nick"]),("color",["red","blue"])]
-- fromList [("color","red"),("color","blue"),("name","Nick")]
fromEntriesByKey :: (ToFormKey k, ToHttpApiData v) => [(k, [v])] -> Form
fromEntriesByKey = Form . Map.fromListWith (<>) . map (toFormKey *** map toQueryParam)

data Proxy3 a b c = Proxy3

type family NotSupported (cls :: k1) (a :: k2) (reason :: Symbol) :: Constraint where
  NotSupported cls a reason = TypeError
    ( 'Text "Cannot derive a Generic-based " ':<>: 'ShowType cls ':<>: 'Text " instance for " ':<>: 'ShowType a ':<>: 'Text "." ':$$:
      'ShowType a ':<>: 'Text " " ':<>: 'Text reason ':<>: 'Text "," ':$$:
      'Text "but Generic-based " ':<>: 'ShowType cls ':<>: 'Text " instances can be derived only for records" ':$$:
      'Text "(i.e. product types with named fields)." )

-- | A 'Generic'-based implementation of 'toForm'.
-- This is used as a default implementation in 'ToForm'.
--
-- Note that this only works for records (i.e. product data types with named fields):
--
-- @
-- data Person = Person
--   { name :: String
--   , age  :: Int
--   } deriving ('Generic')
-- @
--
-- In this implementation each field's value gets encoded using `toQueryParam`.
-- Two field types are exceptions:
--
--    - for values of type @'Maybe' a@ an entry is added to the 'Form' only when it is @'Just' x@
--      and the encoded value is @'toQueryParam' x@; 'Nothing' values are omitted from the 'Form';
--
--    - for values of type @[a]@ (except @['Char']@) an entry is added for every item in the list;
--      if the list is empty no entries are added to the 'Form';
--
-- Here's an example:
--
-- @
-- data Post = Post
--   { title    :: String
--   , subtitle :: Maybe String
--   , comments :: [String]
--   } deriving ('Generic', 'Show')
--
-- instance 'ToForm' Post
-- @
--
-- >>> urlEncodeAsFormStable Post { title = "Test", subtitle = Nothing, comments = ["Nice post!", "+1"] }
-- "comments=Nice%20post%21&comments=%2B1&title=Test"
genericToForm :: forall a. (Generic a, GToForm a (Rep a)) => FormOptions -> a -> Form
genericToForm opts = gToForm (Proxy :: Proxy a) opts . from

class GToForm t (f :: * -> *) where
  gToForm :: Proxy t -> FormOptions -> f x -> Form

instance (GToForm t f, GToForm t g) => GToForm t (f :*: g) where
  gToForm p opts (a :*: b) = gToForm p opts a <> gToForm p opts b

instance (GToForm t f) => GToForm t (M1 D x f) where
  gToForm p opts (M1 a) = gToForm p opts a

instance (GToForm t f) => GToForm t (M1 C x f) where
  gToForm p opts (M1 a) = gToForm p opts a

instance {-# OVERLAPPABLE #-} (Selector s, ToHttpApiData c) => GToForm t (M1 S s (K1 i c)) where
  gToForm _ opts (M1 (K1 c)) = fromList [(key, toQueryParam c)]
    where
      key = Text.pack $ fieldLabelModifier opts $ selName (Proxy3 :: Proxy3 s g p)

instance (Selector s, ToHttpApiData c) => GToForm t (M1 S s (K1 i (Maybe c))) where
  gToForm _ opts (M1 (K1 c)) =
    case c of
      Nothing -> mempty
      Just x  -> fromList [(key, toQueryParam x)]
    where
      key = Text.pack $ fieldLabelModifier opts $ selName (Proxy3 :: Proxy3 s g p)

instance (Selector s, ToHttpApiData c) => GToForm t (M1 S s (K1 i [c])) where
  gToForm _ opts (M1 (K1 cs)) = fromList (map (\c -> (key, toQueryParam c)) cs)
    where
      key = Text.pack $ fieldLabelModifier opts $ selName (Proxy3 :: Proxy3 s g p)

instance {-# OVERLAPPING #-} (Selector s) => GToForm t (M1 S s (K1 i String)) where
  gToForm _ opts (M1 (K1 c)) = fromList [(key, toQueryParam c)]
    where
      key = Text.pack $ fieldLabelModifier opts $ selName (Proxy3 :: Proxy3 s g p)

instance NotSupported ToForm t "is a sum type" => GToForm t (f :+: g) where gToForm = error "impossible"

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
--   'fromForm' f = Person
--     '<$>' 'parseUnique' "name" f
--     '<*>' 'parseUnique' "age"  f
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
-- The default implementation of 'fromForm' is 'genericFromForm'.
-- It only works for records and it will use 'parseQueryParam' for each field's value.
class FromForm a where
  -- | Parse 'Form' into a value.
  fromForm :: Form -> Either Text a
  default fromForm :: (Generic a, GFromForm a (Rep a)) => Form -> Either Text a
  fromForm = genericFromForm defaultFormOptions

instance FromForm Form where fromForm = pure

-- | _NOTE:_ this conversion is unstable and may result in different key order (but not values).
instance (FromFormKey k, FromHttpApiData v) => FromForm [(k, v)] where
  fromForm = fmap (concatMap (\(k, vs) -> map ((,) k) vs)) . toEntriesByKey

instance (Ord k, Eq k, Hashable k, FromFormKey k, FromHttpApiData v) => FromForm (Map.Map k [v]) where
  fromForm = fmap (Map.fromListWith (<>)) . toEntriesByKey

instance FromHttpApiData v => FromForm (IntMap [v]) where
  fromForm = fmap (IntMap.fromListWith (<>)) . toEntriesByKey

-- | Parse a 'Form' into a list of entries groupped by key.
--
-- _NOTE:_ this conversion is unstable and may result in different key order
-- (but not values). For a stable encoding see 'toEntriesByKeyStable'.
toEntriesByKey :: (FromFormKey k, FromHttpApiData v) => Form -> Either Text [(k, [v])]
toEntriesByKey = traverse parseGroup . Map.toList . unForm
  where
    parseGroup (k, vs) = (,) <$> parseFormKey k <*> traverse parseQueryParam vs

-- | Parse a 'Form' into a list of entries groupped by key.
--
-- >>> toEntriesByKeyStable [("name", "Nick"), ("color", "red"), ("color", "white")] :: Either Text [(Text, [Text])]
-- Right [("color",["red","white"]),("name",["Nick"])]
--
-- For an unstable (but faster) conversion see 'toEntriesByKey'.
toEntriesByKeyStable :: (Ord k, FromFormKey k, FromHttpApiData v) => Form -> Either Text [(k, [v])]
toEntriesByKeyStable = fmap (sortOn fst) . toEntriesByKey

-- | A 'Generic'-based implementation of 'fromForm'.
-- This is used as a default implementation in 'FromForm'.
--
-- Note that this only works for records (i.e. product data types with named fields):
--
-- @
-- data Person = Person
--   { name :: String
--   , age  :: Int
--   } deriving ('Generic')
-- @
--
-- In this implementation each field's value gets decoded using `parseQueryParam`.
-- Two field types are exceptions:
--
--    - for values of type @'Maybe' a@ an entry is parsed if present in the 'Form'
--      and the is decoded with 'parseQueryParam'; if no entry is present result is 'Nothing';
--
--    - for values of type @[a]@ (except @['Char']@) all entries are parsed to produce a list of parsed values;
--
-- Here's an example:
--
-- @
-- data Post = Post
--   { title    :: String
--   , subtitle :: Maybe String
--   , comments :: [String]
--   } deriving ('Generic', 'Show')
--
-- instance 'FromForm' Post
-- @
--
-- >>> urlDecodeAsForm "comments=Nice%20post%21&comments=%2B1&title=Test" :: Either Text Post
-- Right (Post {title = "Test", subtitle = Nothing, comments = ["Nice post!","+1"]})
genericFromForm :: forall a. (Generic a, GFromForm a (Rep a)) => FormOptions -> Form -> Either Text a
genericFromForm opts f = to <$> gFromForm (Proxy :: Proxy a) opts f

class GFromForm t (f :: * -> *) where
  gFromForm :: Proxy t -> FormOptions -> Form -> Either Text (f x)

instance (GFromForm t f, GFromForm t g) => GFromForm t (f :*: g) where
  gFromForm p opts f = (:*:) <$> gFromForm p opts f <*> gFromForm p opts f

instance GFromForm t f => GFromForm t (M1 D x f) where
  gFromForm p opts f = M1 <$> gFromForm p opts f

instance GFromForm t f => GFromForm t (M1 C x f) where
  gFromForm p opts f = M1 <$> gFromForm p opts f

instance {-# OVERLAPPABLE #-} (Selector s, FromHttpApiData c) => GFromForm t (M1 S s (K1 i c)) where
  gFromForm _ opts form = M1 . K1 <$> parseUnique key form
    where
      key = Text.pack $ fieldLabelModifier opts $ selName (Proxy3 :: Proxy3 s g p)

instance (Selector s, FromHttpApiData c) => GFromForm t (M1 S s (K1 i (Maybe c))) where
  gFromForm _ opts form = M1 . K1 <$> parseMaybe key form
    where
      key = Text.pack $ fieldLabelModifier opts $ selName (Proxy3 :: Proxy3 s g p)

instance (Selector s, FromHttpApiData c) => GFromForm t (M1 S s (K1 i [c])) where
  gFromForm _ opts form = M1 . K1 <$> parseAll key form
    where
      key = Text.pack $ fieldLabelModifier opts $ selName (Proxy3 :: Proxy3 s g p)

instance {-# OVERLAPPING #-} (Selector s) => GFromForm t (M1 S s (K1 i String)) where
  gFromForm _ opts form = M1 . K1 <$> parseUnique key form
    where
      key = Text.pack $ fieldLabelModifier opts $ selName (Proxy3 :: Proxy3 s g p)

instance NotSupported FromForm t "is a sum type" => GFromForm t (f :+: g) where gFromForm = error "impossible"

-- | Encode a 'Form' to an @application/x-www-form-urlencoded@ 'BSL.ByteString'.
--
-- _NOTE:_ this encoding is unstable and may result in different key order
-- (but not values). For a stable encoding see 'urlEncodeFormStable'.
urlEncodeForm :: Form -> BSL.ByteString
urlEncodeForm = urlEncodeParams . toList

-- | Encode a 'Form' to an @application/x-www-form-urlencoded@ 'BSL.ByteString'.
--
-- For an unstable (but faster) encoding see 'urlEncodeForm'.
--
-- Key-value pairs get encoded to @key=value@ and separated by @&@:
--
-- >>> urlEncodeFormStable [("name", "Julian"), ("lastname", "Arni")]
-- "lastname=Arni&name=Julian"
--
-- Keys with empty values get encoded to just @key@ (without the @=@ sign):
--
-- >>> urlEncodeFormStable [("is_test", "")]
-- "is_test"
--
-- Empty keys are allowed too:
--
-- >>> urlEncodeFormStable [("", "foobar")]
-- "=foobar"
--
-- However, if both key and value are empty, the key-value pair is ignored.
-- (This prevents @'urlDecodeForm' . 'urlEncodeFormStable'@ from being a true isomorphism).
--
-- >>> urlEncodeFormStable [("", "")]
-- ""
--
-- Everything is escaped with @'escapeURIString' 'isUnreserved'@:
--
-- >>> urlEncodeFormStable [("fullname", "Andres Löh")]
-- "fullname=Andres%20L%C3%B6h"
urlEncodeFormStable :: Form -> BSL.ByteString
urlEncodeFormStable = urlEncodeParams . sortOn fst . toList

-- | Encode a list of key-value pairs to an @application/x-www-form-urlencoded@ 'BSL.ByteString'.
--
-- See also 'urlEncodeFormStable'.
urlEncodeParams :: [(Text, Text)] -> BSL.ByteString
urlEncodeParams = toLazyByteString . mconcat . intersperse (shortByteString "&") . map encodePair
  where
    escape = urlEncodeBuilder True . Text.encodeUtf8

    encodePair (k, "") = escape k
    encodePair (k, v)  = escape k <> shortByteString "=" <> escape v

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
urlDecodeForm = fmap toForm . urlDecodeParams

-- | Decode an @application/x-www-form-urlencoded@ 'BSL.ByteString' to a list of key-value pairs.
--
-- See also 'urlDecodeForm'.
urlDecodeParams :: BSL.ByteString -> Either Text [(Text, Text)]
urlDecodeParams bs = traverse parsePair pairs
  where
    pairs = map (BSL8.split '=') (BSL8.split '&' bs)

    unescape = Text.decodeUtf8With lenientDecode . urlDecode True . BSL.toStrict

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
-- _NOTE:_ this encoding is unstable and may result in different key order
-- (but not values). For a stable encoding see 'urlEncodeAsFormStable'.
urlEncodeAsForm :: ToForm a => a -> BSL.ByteString
urlEncodeAsForm = urlEncodeForm . toForm

-- | This is a convenience function for encoding a datatype that has instance
-- of 'ToForm' directly to a @application/x-www-form-urlencoded@
-- 'BSL.ByteString'.
--
-- This is effectively @'urlEncodeFormStable' . 'toForm'@.
--
-- >>> urlEncodeAsFormStable Person {name = "Dennis", age = 22}
-- "age=22&name=Dennis"
urlEncodeAsFormStable :: ToForm a => a -> BSL.ByteString
urlEncodeAsFormStable = urlEncodeFormStable . toForm

-- | Find all values corresponding to a given key in a 'Form'.
--
-- >>> lookupAll "name" []
-- []
-- >>> lookupAll "name" [("name", "Oleg")]
-- ["Oleg"]
-- >>> lookupAll "name" [("name", "Oleg"), ("name", "David")]
-- ["Oleg","David"]
lookupAll :: Text -> Form -> [Text]
lookupAll key = F.concat . Map.lookup key . unForm

-- | Lookup an optional value for a key.
-- Fail if there is more than one value.
--
-- >>> lookupMaybe "name" []
-- Right Nothing
-- >>> lookupMaybe "name" [("name", "Oleg")]
-- Right (Just "Oleg")
-- >>> lookupMaybe "name" [("name", "Oleg"), ("name", "David")]
-- Left "Duplicate key \"name\""
lookupMaybe :: Text -> Form -> Either Text (Maybe Text)
lookupMaybe key form =
  case lookupAll key form of
    []  -> pure Nothing
    [v] -> pure (Just v)
    _   -> Left $ "Duplicate key " <> Text.pack (show key)

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
lookupUnique key form = do
  mv <- lookupMaybe key form
  case mv of
    Just v  -> pure v
    Nothing -> Left $ "Could not find key " <> Text.pack (show key)

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

-- | Lookup an optional value for a given key and parse it with 'parseQueryParam'.
-- Fail if there is more than one value for the key.
--
-- >>> parseMaybe "age" [] :: Either Text (Maybe Word8)
-- Right Nothing
-- >>> parseMaybe "age" [("age", "12"), ("age", "25")] :: Either Text (Maybe Word8)
-- Left "Duplicate key \"age\""
-- >>> parseMaybe "age" [("age", "seven")] :: Either Text (Maybe Word8)
-- Left "could not parse: `seven' (input does not start with a digit)"
-- >>> parseMaybe "age" [("age", "777")] :: Either Text (Maybe Word8)
-- Left "out of bounds: `777' (should be between 0 and 255)"
-- >>> parseMaybe "age" [("age", "7")] :: Either Text (Maybe Word8)
-- Right (Just 7)
parseMaybe :: FromHttpApiData v => Text -> Form -> Either Text (Maybe v)
parseMaybe key = parseQueryParams <=< lookupMaybe key

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

-- | 'Generic'-based deriving options for 'ToForm' and 'FromForm'.
--
-- A common use case for non-default 'FormOptions'
-- is to strip a prefix off of field labels:
--
-- @
-- data Project = Project
--   { projectName :: String
--   , projectSize :: Int
--   } deriving ('Generic', 'Show')
--
-- myOptions :: 'FormOptions'
-- myOptions = 'FormOptions'
--  { 'fieldLabelModifier' = 'map' 'toLower' . 'drop' ('length' \"project\") }
--
-- instance 'ToForm' Project where
--   'toForm' = 'genericToForm' myOptions
--
-- instance 'FromForm' Project where
--   'fromForm' = 'genericFromForm' myOptions
-- @
--
-- >>> urlEncodeAsFormStable Project { projectName = "http-api-data", projectSize = 172 }
-- "name=http-api-data&size=172"
-- >>> urlDecodeAsForm "name=http-api-data&size=172" :: Either Text Project
-- Right (Project {projectName = "http-api-data", projectSize = 172})
data FormOptions = FormOptions
  { -- | Function applied to field labels. Handy for removing common record prefixes for example.
    fieldLabelModifier :: String -> String
  }

-- | Default encoding 'FormOptions'.
--
-- @
-- 'FormOptions'
-- { 'fieldLabelModifier' = id
-- }
-- @
defaultFormOptions :: FormOptions
defaultFormOptions = FormOptions
  { fieldLabelModifier = id
  }

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = sortBy (comparing f)
