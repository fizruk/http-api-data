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

import           Control.Arrow             (first, left, second)
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

-- | The contents of a form, not yet url-encoded.
newtype Form = Form { unForm :: M.Map T.Text T.Text }
  deriving (Eq, Show, Read, Generic, Monoid)

instance IsList Form where
  type Item Form = (T.Text, T.Text)
  fromList = Form . M.fromList
  toList = M.toList . unForm

-- | A type that can be converted to @application/x-www-form-urlencoded@
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

-- | A type that can be converted from @application/x-www-form-urlencoded@,
-- with the possibility of failure.
class FromForm a where
  fromForm :: Form -> Either String a
  default fromForm :: (Generic a, GFromForm (Rep a))
     => Form -> Either String a
  fromForm = genericFromForm

instance FromForm Form where fromForm = return
instance FromForm [(T.Text, T.Text)] where fromForm = return . M.toList . unForm
instance FromForm (M.Map T.Text T.Text) where fromForm = return . unForm

genericFromForm :: (Generic a, GFromForm (Rep a))
    => Form -> Either String a
genericFromForm f = to <$> gFromForm f

class GFromForm (f :: * -> *) where
  gFromForm :: Form -> Either String (f x)

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
    Nothing -> Left . T.unpack $ "Could not find key " <> sel
    Just v  -> left T.unpack $ M1 . K1 <$> parseQueryParam v
    where sel = T.pack $ selName (Proxy3 :: Proxy3 sel g p)

instance (GFromForm f) => GFromForm (M1 D x f) where
  gFromForm f = M1 <$> gFromForm f

instance (GFromForm f) => GFromForm (M1 C x f) where
  gFromForm f = M1 <$> gFromForm f

encodeForm :: Form -> BSL.ByteString
encodeForm xs =
    let escape :: T.Text -> BSL.ByteString
        escape = BSLUTF8.fromString . escapeURIString isUnreserved . T.unpack
        encodePair :: (T.Text, T.Text) -> BSL.ByteString
        encodePair (k, "") = escape k
        encodePair (k, v) = escape k <> "=" <> escape v
    in BSL.intercalate "&" $ map encodePair $ toList xs

decodeForm :: BSL.ByteString -> Either String Form
decodeForm "" = return mempty
decodeForm q = do
    let xs :: [T.Text]
        xs = T.splitOn "&" . decodeUtf8With lenientDecode . mconcat . BSL.toChunks $ q
        parsePair :: T.Text -> Either String (T.Text, T.Text)
        parsePair p =
            case T.splitOn "=" p of
                [k,v] -> return ( unescape k
                                , unescape v
                                )
                [k] -> return ( unescape k, "" )
                _ -> Left $ "not a valid pair: " <> T.unpack p
        unescape :: T.Text -> T.Text
        unescape = T.pack . unEscapeString . T.unpack . T.intercalate "%20" . T.splitOn "+"
    toForm <$> mapM parsePair xs

data Proxy3 a b c = Proxy3
