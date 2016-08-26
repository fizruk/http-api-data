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
-- >>> :set -XScopedTypeVariables
-- >>> :set -XTypeFamilies
-- >>> import Data.Either (isLeft)
-- >>> import Data.List (sort)

-- | The contents of a form, not yet url-encoded.
--
-- Forms can be URL-encoded with 'encodeForm'.  Url-encoded bytestrings can be
-- converted to a 'Form' with 'decodeForm'.
newtype Form = Form { unForm :: M.Map T.Text T.Text }
  deriving (Eq, Show, Read, Generic, Monoid)

-- | Convert a list of @('T.Text', 'T.Text')@ to a 'Form'.
--
-- >>> let (form :: Form) = fromList [("baz", "qux"), ("foo", "bar")]
-- >>> toList form
-- [("baz","qux"),("foo","bar")]
instance IsList Form where
  type Item Form = (T.Text, T.Text)
  fromList = Form . M.fromList
  toList = M.toList . unForm

-- | A type that can be converted to a 'Form'.
--
-- >>> let form = toForm [("foo" :: T.Text, "bar" :: T.Text)]
-- >>> encodeForm form
-- "foo=bar"
--
-- Your own datatypes can be converted to a 'Form' like the following:
--
-- >>> data MyFoo = MyFoo { myFooBar :: T.Text }
-- >>> instance ToForm MyFoo where toForm (MyFoo bar') = fromList [("bar", bar')]
-- >>> let form = toForm $ MyFoo { myFooBar = "hello" }
-- >>> encodeForm form
-- "bar=hello"
--
-- There is a default method for 'toForm' for any Haskell record type that
-- derives 'Generic'.  The record's fields will be turned into the keys of the
-- 'Form'.  This only works for fields with types that implement
-- 'ToHttpApiData'.
--
-- This can be used like the following:
--
-- >>> data MyGenericBar = MyGenericBar { myGenericBar :: Int } deriving Generic
-- >>> instance ToForm MyGenericBar
-- >>> let form = toForm $ MyGenericBar { myGenericBar = 3 }
-- >>> encodeForm form
-- "myGenericBar=3"
class ToForm a where
  toForm :: a -> Form
  default toForm :: (Generic a, GToForm (Rep a)) => a -> Form
  toForm = genericToForm

instance ToForm [(T.Text, T.Text)] where toForm = Form . M.fromList
instance ToForm (M.Map T.Text T.Text) where toForm = Form
instance ToForm Form where toForm = id

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

-- | A type that can be converted from a 'Form', with the possibility of
-- failure.
--
-- >>> let form = fromList [("foo" :: T.Text, "bar" :: T.Text)]
-- >>> fromForm form :: Either T.Text [(T.Text, T.Text)]
-- Right [("foo","bar")]
--
-- Your own datatypes can be converted from a 'Form' like the following:
--
-- >>> data MyFoo = MyFoo { myFooBar :: T.Text } deriving Show
-- >>> instance FromForm MyFoo where fromForm (Form map') = maybe (Left "key \"bar\" not found") (Right . MyFoo) $ M.lookup "bar" map'
-- >>> let form = fromList [("bar" :: T.Text, "hello" :: T.Text)]
-- >>> fromForm form :: Either T.Text MyFoo
-- Right (MyFoo {myFooBar = "hello"})
--
-- There is a default method for 'fromForm' for any Haskell record type that
-- derives 'Generic'.  The record's fields will be taken from the keys of the
-- 'Form'.  This only works for fields with types that implement
-- 'FromHttpApiData'.
--
-- This can be used like the following:
--
-- >>> data MyGenericBar = MyGenericBar { myGenericBar :: Int } deriving (Generic, Show)
-- >>> instance FromForm MyGenericBar
-- >>> let form = fromList [("myGenericBar" :: T.Text, "1" :: T.Text)]
-- >>> fromForm form :: Either T.Text MyGenericBar
-- Right (MyGenericBar {myGenericBar = 1})
-- >>> let form = fromList [("some other key" :: T.Text, "2" :: T.Text)]
-- >>> isLeft (fromForm form :: Either T.Text MyGenericBar)
-- True
class FromForm a where
  fromForm :: Form -> Either T.Text a
  default fromForm :: (Generic a, GFromForm (Rep a))
     => Form -> Either T.Text a
  fromForm = genericFromForm

instance FromForm Form where fromForm = return
instance FromForm [(T.Text, T.Text)] where fromForm = return . M.toList . unForm
instance FromForm (M.Map T.Text T.Text) where fromForm = return . unForm

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

-- | Encode a 'Form' to a @application/x-www-form-urlencoded@ 'BSL.ByteString'.
--
-- Key-value pairs get encoded to @key=value@.
--
-- >>> encodeForm $ toForm [("foo" :: T.Text, "bar" :: T.Text)]
-- "foo=bar"
--
-- Keys with no values get encoded to just @key@.
--
-- >>> encodeForm $ toForm [("foo" :: T.Text, "" :: T.Text)]
-- "foo"
--
-- Empty keys don't show up.
--
-- >>> encodeForm $ toForm [("" :: T.Text, "bar" :: T.Text)]
-- "=bar"
--
-- When both they key and value is empty, it gets encoded as nothing.  (This
-- prevents @'decodeForm' . 'encodeForm'@ from being an isomorphism).
--
-- >>> encodeForm $ toForm [("" :: T.Text, "" :: T.Text)]
-- ""
--
-- Other characters are escaped by @'escapeURIString' 'isUnreserved'@.
--
-- >>> encodeForm $ toForm [("foo" :: T.Text, "value with spaces" :: T.Text)]
-- "foo=value%20with%20spaces"
encodeForm :: Form -> BSL.ByteString
encodeForm xs =
    let escape :: T.Text -> BSL.ByteString
        escape = BSLUTF8.fromString . escapeURIString isUnreserved . T.unpack
        encodePair :: (T.Text, T.Text) -> BSL.ByteString
        encodePair (k, "") = escape k
        encodePair (k, v) = escape k <> "=" <> escape v
    in BSL.intercalate "&" $ map encodePair $ toList xs

-- | Decode a @application/x-www-form-urlencoded@ 'BSL.ByteString' to a 'Form'.
--
-- Key-value pairs get decoded normally.
--
-- >>> toList <$> decodeForm "foo=bar"
-- Right [("foo","bar")]
-- >>> sort . toList <$> decodeForm "foo=bar&abc=xyz"
-- Right [("abc","xyz"),("foo","bar")]
--
-- Keys with no values get decoded to empty values.
--
-- >>> toList <$> decodeForm "foo"
-- Right [("foo","")]
--
-- Empty keys get decoded as @""@.
--
-- >>> toList <$> decodeForm "=bar"
-- Right [("","bar")]
--
-- The empty string gets decoded as no key-value pairs.
--
-- >>> toList <$> decodeForm ""
-- Right []
--
-- Other characters are un-escaped with 'unEscapeString'.
--
-- >>> toList <$> decodeForm "foo=value%20with%20spaces"
-- Right [("foo","value with spaces")]
--
-- Improperly formed strings return 'Left'.
--
-- >>> isLeft $ decodeForm "this=has=too=many=equals"
-- True
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
