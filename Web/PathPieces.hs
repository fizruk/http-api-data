{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, OverloadedStrings #-}
module Web.PathPieces
    ( PathPiece (..)
    , PathMultiPiece (..)
    , readFromPathPiece
    , showToPathPiece
    -- * Deprecated
    , toSinglePiece
    , toMultiPiece
    , fromSinglePiece
    , fromMultiPiece
    ) where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import qualified Data.Text as S
import qualified Data.Text.Lazy as L
import qualified Data.Text.Read
import Data.Time (Day)
import Control.Exception (assert)
import Text.Read (readMaybe)

class PathPiece s where
    fromPathPiece :: S.Text -> Maybe s
    toPathPiece :: s -> S.Text

instance PathPiece () where
    fromPathPiece t = if t == "_" then Just () else Nothing
    toPathPiece () = "_"

instance PathPiece String where
    fromPathPiece = Just . S.unpack
    toPathPiece = S.pack

instance PathPiece S.Text where
    fromPathPiece = Just
    toPathPiece = id

instance PathPiece L.Text where
    fromPathPiece = Just . L.fromChunks . return
    toPathPiece = S.concat . L.toChunks

parseIntegral :: (Integral a, Bounded a, Ord a) => S.Text -> Maybe a
parseIntegral s = n
    where
    n = case Data.Text.Read.signed Data.Text.Read.decimal s of
        Right (i, "") | i <= top && i >= bot -> Just (fromInteger i)
        _ -> Nothing
    Just witness = n
    top = toInteger (maxBound `asTypeOf` witness)
    bot = toInteger (minBound `asTypeOf` witness)

instance PathPiece Integer where
    fromPathPiece s =
        case Data.Text.Read.signed Data.Text.Read.decimal s of
            Right (i, "") -> Just i
            _ -> Nothing
    toPathPiece = S.pack . show

instance PathPiece Int where
    fromPathPiece = parseIntegral
    toPathPiece = S.pack . show

instance PathPiece Int8 where
    fromPathPiece = parseIntegral
    toPathPiece = S.pack . show

instance PathPiece Int16 where
    fromPathPiece = parseIntegral
    toPathPiece = S.pack . show

instance PathPiece Int32 where
    fromPathPiece = parseIntegral
    toPathPiece = S.pack . show

instance PathPiece Int64 where
    fromPathPiece = parseIntegral
    toPathPiece = S.pack . show

instance PathPiece Word where
    fromPathPiece = parseIntegral
    toPathPiece = S.pack . show

instance PathPiece Word8 where
    fromPathPiece = parseIntegral
    toPathPiece = S.pack . show

instance PathPiece Word16 where
    fromPathPiece = parseIntegral
    toPathPiece = S.pack . show

instance PathPiece Word32 where
    fromPathPiece = parseIntegral
    toPathPiece = S.pack . show

instance PathPiece Word64 where
    fromPathPiece = parseIntegral
    toPathPiece = S.pack . show

instance PathPiece Bool where
    fromPathPiece t =
        case filter (null . snd) $ reads $ S.unpack t of
            (a, s):_ -> assert (null s) (Just a)
            _        -> Nothing
    toPathPiece = S.pack . show

instance PathPiece Day where
    fromPathPiece t =
        case reads $ S.unpack t of
            [(a,"")] -> Just a
            _ -> Nothing
    toPathPiece = S.pack . show

instance (PathPiece a) => PathPiece (Maybe a) where
    fromPathPiece s = case S.stripPrefix "Just " s of
        Just r -> Just `fmap` fromPathPiece r
        _ -> case s of
            "Nothing" -> Just Nothing
            _ -> Nothing
    toPathPiece m = case m of
        Just s -> "Just " `S.append` toPathPiece s
        _ -> "Nothing"

class PathMultiPiece s where
    fromPathMultiPiece :: [S.Text] -> Maybe s
    toPathMultiPiece :: s -> [S.Text]

instance PathPiece a => PathMultiPiece [a] where
    fromPathMultiPiece = mapM fromPathPiece
    toPathMultiPiece = map toPathPiece

-- | A function for helping generate free 'PathPiece'
--   instances for enumeration data types 
--   that have derived 'Read' and 'Show' instances.
--   Intended to be used like this:
--
--   > data MyData = Foo | Bar | Baz
--   >   deriving (Read,Show)
--   > instance PathPiece MyData where
--   >   fromPathPiece = readFromPathPiece
--   >   toPathPiece = showToPathPiece
--
--  Since 0.2.1. 
readFromPathPiece :: Read s => S.Text -> Maybe s
readFromPathPiece = readMaybe . S.unpack

-- | See the documentation for 'readFromPathPiece'.
--
--  Since 0.2.1. 
showToPathPiece :: Show s => s -> S.Text
showToPathPiece = S.pack . show

{-# DEPRECATED toSinglePiece "Use toPathPiece instead of toSinglePiece" #-}
toSinglePiece :: PathPiece p => p -> S.Text
toSinglePiece = toPathPiece

{-# DEPRECATED fromSinglePiece "Use fromPathPiece instead of fromSinglePiece" #-}
fromSinglePiece :: PathPiece p => S.Text -> Maybe p
fromSinglePiece = fromPathPiece

{-# DEPRECATED toMultiPiece "Use toPathMultiPiece instead of toMultiPiece" #-}
toMultiPiece :: PathMultiPiece ps => ps -> [S.Text]
toMultiPiece = toPathMultiPiece

{-# DEPRECATED fromMultiPiece "Use fromPathMultiPiece instead of fromMultiPiece" #-}
fromMultiPiece :: PathMultiPiece ps => [S.Text] -> Maybe ps
fromMultiPiece = fromPathMultiPiece
