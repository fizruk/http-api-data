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
module Web.HttpApiData.Internal.FormUrlEncoded where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import           Control.Arrow             (first, second)
import           Control.Monad.State
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSLUTF8
import qualified Data.Map                  as M
import           Data.Monoid
import qualified Data.Text                 as T
import           Data.Text.Encoding        (decodeUtf8With)
import           Data.Text.Encoding.Error  (lenientDecode)
import           GHC.Exts                  (IsList (..))
import           GHC.Generics
import           Network.URI               (escapeURIString, isUnreserved,
                                            unEscapeString)

import Web.HttpApiData.Internal.HttpApiData

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

-- | The contents of a form, not yet URL-encoded.
--
-- 'Form' can be URL-encoded with 'encodeForm' and URL-decoded with 'decodeForm'.
newtype Form = Form { unForm :: M.Map T.Text T.Text }
  deriving (Eq, Show, Read, Generic, Monoid)

instance IsList Form where
  type Item Form = (T.Text, T.Text)
  fromList = Form . M.fromList
  toList = M.toList . unForm

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

instance ToForm [(T.Text, T.Text)] where toForm = Form . M.fromList
instance ToForm (M.Map T.Text T.Text) where toForm = Form
instance ToForm Form where toForm = id

-- | A 'Generic'-based implementation of 'toForm'.
-- This is used as a default implementation in 'ToForm'.
genericToForm :: (Generic a, GToForm (Rep a)) => a -> Form
genericToForm x = evalState (unGenericToFormS . gToForm $ from x) (Nothing, 0)

-- | A datatype for generically generating forms. The state keeps track of the
-- record name currently being inspected (if any) and the field number.
newtype GenericToFormS a = GenericToFormS {
  unGenericToFormS :: State (Maybe T.Text, Int) a
  } deriving (Functor, Applicative, Generic, Monad, MonadState (Maybe T.Text, Int))

class GToForm (f :: * -> *) where
  gToForm :: f x -> GenericToFormS Form

instance (GToForm f, GToForm g) => GToForm (f :*: g) where
  gToForm (a :*: b) = do
    a' <- gToForm a
    modify $ second (+ 1)
    b' <- gToForm b
    return (a' <> b')

instance (GToForm f, GToForm g) => GToForm (f :+: g) where
  gToForm (L1 a) = gToForm a
  gToForm (R1 a) = gToForm a

instance (GToForm f) => GToForm (M1 D x f) where
  gToForm (M1 a) = gToForm a

instance (GToForm f) => GToForm (M1 C x f) where
  gToForm (M1 a) = gToForm a

instance {-# OVERLAPPABLE #-} (Selector sel, GToForm f)
    => GToForm (M1 S sel f) where
  gToForm (M1 a) = modify (first $ const sel) >> gToForm a
    where sel = Just . T.pack $ selName (Proxy3 :: Proxy3 sel g p)

instance (ToHttpApiData f) => GToForm (K1 i f) where
  gToForm (K1 a) = do
    s <- get
    return . Form $ M.insert (name s) (toQueryParam a) $ mempty
    where name (Nothing, i) = T.pack $ show i
          name (Just x,  _) = x

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
  fromForm :: Form -> Either T.Text a
  default fromForm :: (Generic a, GFromForm (Rep a))
     => Form -> Either T.Text a
  fromForm = genericFromForm

instance FromForm Form where fromForm = return
instance FromForm [(T.Text, T.Text)] where fromForm = return . M.toList . unForm
instance FromForm (M.Map T.Text T.Text) where fromForm = return . unForm

-- | A 'Generic'-based implementation of 'fromForm'.
-- This is used as a default implementation in 'FromForm'.
genericFromForm :: (Generic a, GFromForm (Rep a))
    => Form -> Either T.Text a
genericFromForm f = to <$> gFromForm f

class GFromForm (f :: * -> *) where
  gFromForm :: Form -> Either T.Text (f x)

instance (GFromForm f, GFromForm g)
    => GFromForm (f :*: g) where
  gFromForm f = (:*:) <$> gFromForm f <*> gFromForm f

instance (GFromForm f, GFromForm g)
    => GFromForm (f :+: g) where
  gFromForm f = case gFromForm f of
    Right a -> return $ L1 a
    Left e1 -> case gFromForm f of
      Right b -> return $ R1 b
      Left e2 -> Left (e1 <> e2)

instance (Selector sel, FromHttpApiData f)
    => GFromForm (M1 S sel (K1 i f)) where
  gFromForm f = case M.lookup sel (unForm f) of
    Nothing -> Left $ "Could not find key " <> sel
    Just v  -> M1 . K1 <$> parseQueryParam v
    where sel = T.pack $ selName (Proxy3 :: Proxy3 sel g p)

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
encodeForm xs =
    let escape :: T.Text -> BSL.ByteString
        escape = BSLUTF8.fromString . escapeURIString isUnreserved . T.unpack
        encodePair :: (T.Text, T.Text) -> BSL.ByteString
        encodePair (k, "") = escape k
        encodePair (k, v) = escape k <> "=" <> escape v
    in BSL.intercalate "&" $ map encodePair $ toList xs

-- | Decode an @application/x-www-form-urlencoded@ 'BSL.ByteString' to a 'Form'.
--
-- Key-value pairs get decoded normally:
--
-- >>> toList <$> decodeForm "name=Greg&lastname=Weber"
-- Right [("lastname","Weber"),("name","Greg")]
--
-- Keys with no values get decoded to pairs with empty values.
--
-- >>> toList <$> decodeForm "is_test"
-- Right [("is_test","")]
--
-- Empty keys are allowed:
--
-- >>> toList <$> decodeForm "=foobar"
-- Right [("","foobar")]
--
-- The empty string gets decoded into an empty 'Form':
--
-- >>> toList <$> decodeForm ""
-- Right []
--
-- Everything is un-escaped with 'unEscapeString':
--
-- >>> toList <$> decodeForm "fullname=Andres%20L%C3%B6h"
-- Right [("fullname","Andres L\246h")]
--
-- Improperly formed strings result in an error:
--
-- >>> decodeForm "this=has=too=many=equals"
-- Left "not a valid pair: this=has=too=many=equals"
decodeForm :: BSL.ByteString -> Either T.Text Form
decodeForm "" = return mempty
decodeForm q = do
    let xs :: [T.Text]
        xs = T.splitOn "&" . decodeUtf8With lenientDecode . mconcat . BSL.toChunks $ q
        parsePair :: T.Text -> Either T.Text (T.Text, T.Text)
        parsePair p =
            case T.splitOn "=" p of
                [k,v] -> return ( unescape k
                                , unescape v
                                )
                [k] -> return ( unescape k, "" )
                _ -> Left $ "not a valid pair: " <> p
        unescape :: T.Text -> T.Text
        unescape = T.pack . unEscapeString . T.unpack . T.intercalate "%20" . T.splitOn "+"
    toForm <$> mapM parsePair xs

data Proxy3 a b c = Proxy3

-- | This is a convenience function for decoding a
-- @application/x-www-form-urlencoded@ 'BSL.ByteString' directly to a datatype
-- that has an instance for 'FromForm'.
--
-- This is effectively @'fromForm' '<=<' 'decodeForm'@.
--
-- >>> decodeWithFromForm "name=Dennis&age=22" :: Either T.Text Person
-- Right (Person {name = "Dennis", age = 22})
decodeWithFromForm :: FromForm a => BSL.ByteString -> Either T.Text a
decodeWithFromForm = fromForm <=< decodeForm

-- | This is a convenience function for encoding a datatype that has instance
-- for 'ToForm' directly to a @application/x-www-form-urlencoded@
-- 'BSL.ByteString'.
--
-- This is effectively @'encodeForm' . 'toForm'@.
--
-- >>> encodeWithToForm Person {name = "Dennis", age = 22}
-- "age=22&name=Dennis"
encodeWithToForm :: ToForm a => a -> BSL.ByteString
encodeWithToForm = encodeForm . toForm
