{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
module Web.HttpApiData.Internal.FormUrlEncoded where

import           Control.Arrow                        (first, second)
import           Control.Monad.State
import qualified Data.HashMap                         as H
import           Data.Monoid
import qualified Data.Text                            as T
import           GHC.Generics

import           Web.HttpApiData.Internal.HttpApiData

-- | The contents of a form, not yet url-encoded.
newtype Form = Form { unForm :: H.HashMap T.Text (Maybe T.Text) }
  deriving (Eq, Show, Read, Generic, Monoid)

-- | A type that can be converted to @application/x-www-form-urlencoded@
class ToFormUrlEncoded a where
  toForm :: a -> Form

instance ToFormUrlEncoded [(T.Text, Maybe T.Text)] where
  toForm = Form . H.fromList

instance ToFormUrlEncoded (H.HashMap T.Text (Maybe T.Text)) where
  toForm = Form

-- | A datatype for generically generating forms. The state keep track of the
-- record name currently being inspected (if any) and the field number.
newtype GenericToFormS a = GenericToFormS {
  unGenericToFormS :: State (Maybe T.Text, Int) a
  } deriving (Functor, Applicative, Generic, Monad, MonadState (Maybe T.Text, Int))

class GToFormUrlEncoded (f :: * -> *) where
  gToForm :: f x -> GenericToFormS Form

instance (GToFormUrlEncoded f, GToFormUrlEncoded g) => GToFormUrlEncoded (f :*: g) where
  gToForm (a :*: b) = do
    a' <- gToForm a
    modify $ second (+ 1)
    b' <- gToForm b
    return (a' <> b')

instance (GToFormUrlEncoded f, GToFormUrlEncoded g) => GToFormUrlEncoded (f :+: g) where
  gToForm (L1 a) = gToForm a
  gToForm (R1 a) = gToForm a

instance (GToFormUrlEncoded f) => GToFormUrlEncoded (M1 S NoSelector f) where
  gToForm (M1 a) = gToForm a

instance (Selector sel, GToFormUrlEncoded f) => GToFormUrlEncoded (M1 S sel f) where
  gToForm (M1 a) = modify (first $ const sel) >> gToForm a
    where sel = Just . T.pack $ selName (Proxy3 :: Proxy3 sel g p)

instance (ToHttpApiData f) => GToFormUrlEncoded (K1 i f) where
  gToForm (K1 a) = do
    s <- get
    return . Form $ H.insert (name s) (Just $ toUrlPiece a) $ mempty
    where name (Nothing, i) = T.pack $ show i
          name (Just x,  _) = x

{-genericToFormUrlEncoded :: GToFormUrlEncoded a => a -> H.HashMap T.Text (Maybe T.Text)-}
{-genericToFormUrlEncoded a = -}

-- | A type that can be converted from @application/x-www-form-urlencoded@,
-- with the possibility of failure.
class FromFormUrlEncoded a where
  fromFormUrlEncoded :: [(T.Text, T.Text)] -> Either String a

instance FromFormUrlEncoded [(T.Text, T.Text)] where
  fromFormUrlEncoded = return

{-
encodeFormUrlEncoded :: [(T.Text, T.Text)] -> ByteString
encodeFormUrlEncoded xs =
    let escape :: T.Text -> ByteString
        escape = cs . escapeURIString isUnreserved . cs
        encodePair :: (T.Text, T.Text) -> ByteString
        encodePair (k, "") = escape k
        encodePair (k, v) = escape k <> "=" <> escape v
    in B.intercalate "&" $ map encodePair xs

decodeFormUrlEncoded :: ByteString -> Either String [(T.Text, T.Text)]
decodeFormUrlEncoded "" = return []
decodeFormUrlEncoded q = do
    let xs :: [T.Text]
        xs = T.splitOn "&" . cs $ q
        parsePair :: T.Text -> Either String (T.Text, T.Text)
        parsePair p =
            case T.splitOn "=" p of
                [k,v] -> return ( unescape k
                                , unescape v
                                )
                [k] -> return ( unescape k, "" )
                _ -> Left $ "not a valid pair: " <> cs p
        unescape :: T.Text -> T.Text
        unescape = cs . unEscapeString . cs . T.intercalate "%20" . T.splitOn "+"
    mapM parsePair xs
-}
data Proxy3 a b c = Proxy3
