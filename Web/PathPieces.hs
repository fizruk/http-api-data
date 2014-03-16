{-# LANGUAGE FlexibleInstances, TypeSynonymInstances  #-}
module Web.PathPieces
    ( PathPiece (..)
    , PathMultiPiece (..)
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

class PathPiece s where
    fromPathPiece :: S.Text -> Maybe s
    toPathPiece :: s -> S.Text

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
        Right (i, _) | i <= top && i >= bot -> Just (fromInteger i)
        _ -> Nothing
    Just witness = n
    top = toInteger (maxBound `asTypeOf` witness)
    bot = toInteger (minBound `asTypeOf` witness)

instance PathPiece Integer where
    fromPathPiece s = case Data.Text.Read.signed Data.Text.Read.decimal s of
        Right (i, _) -> Just i
        Left _ -> Nothing
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

instance PathPiece Day where
    fromPathPiece t =
        case reads $ S.unpack t of
            [(a,"")] -> Just a
            _ -> Nothing
    toPathPiece = S.pack . show

class PathMultiPiece s where
    fromPathMultiPiece :: [S.Text] -> Maybe s
    toPathMultiPiece :: s -> [S.Text]

instance PathMultiPiece [String] where
    fromPathMultiPiece = Just . map S.unpack
    toPathMultiPiece = map S.pack

instance PathMultiPiece [S.Text] where
    fromPathMultiPiece = Just
    toPathMultiPiece = id

instance PathMultiPiece [L.Text] where
    fromPathMultiPiece = Just . map (L.fromChunks . return)
    toPathMultiPiece = map $ S.concat . L.toChunks

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
