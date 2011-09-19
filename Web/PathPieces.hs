{-# LANGUAGE FlexibleInstances, TypeSynonymInstances  #-}
module Web.PathPieces
    ( RoutePiece (..)
    , RouteMultiPiece (..)
    -- * Deprecated
    , SinglePiece (..)
    , MultiPiece (..)
    ) where

import Data.Int (Int64)
import qualified Data.Text as S
import qualified Data.Text.Lazy as L
import qualified Data.Text.Read

class RoutePiece s where
    fromRoutePiece :: S.Text -> Maybe s
    toRoutePiece :: s -> S.Text

instance RoutePiece String where
    fromRoutePiece s = if S.null s then Nothing else Just (S.unpack s)
    toRoutePiece = S.pack

instance RoutePiece S.Text where
    fromRoutePiece s = if S.null s then Nothing else Just s
    toRoutePiece = id

instance RoutePiece L.Text where
    fromRoutePiece s = if S.null s then Nothing else Just (L.fromChunks [s])
    toRoutePiece = S.concat . L.toChunks

instance RoutePiece Integer where
    fromRoutePiece s =
        case Data.Text.Read.decimal s of
            Right (i, _) -> Just i
            Left _ -> Nothing
    toRoutePiece = S.pack . show

instance RoutePiece Int where
    fromRoutePiece s =
        case Data.Text.Read.decimal s of
            Right (i, _) -> Just i
            Left _ -> Nothing
    toRoutePiece = S.pack . show

instance RoutePiece Int64 where
    fromRoutePiece s =
        case Data.Text.Read.decimal s of
            Right (i, _) -> Just i
            Left _ -> Nothing
    toRoutePiece = S.pack . show


class RouteMultiPiece s where
    fromRouteMultiPiece :: [S.Text] -> Maybe s
    toRouteMultiPiece :: s -> [S.Text]

instance RouteMultiPiece [String] where
    fromRouteMultiPiece = Just . map S.unpack
    toRouteMultiPiece = map S.pack

instance RouteMultiPiece [S.Text] where
    fromRouteMultiPiece = Just
    toRouteMultiPiece = id

instance RouteMultiPiece [L.Text] where
    fromRouteMultiPiece = Just . map (L.fromChunks . return)
    toRouteMultiPiece = map $ S.concat . L.toChunks


-- | deprecated api
class SinglePiece s where
    fromSinglePiece :: S.Text -> Maybe s
    toSinglePiece :: s -> S.Text

instance SinglePiece String where
    fromSinglePiece s = if S.null s then Nothing else Just (S.unpack s)
    toSinglePiece = S.pack

instance SinglePiece S.Text where
    fromSinglePiece s = if S.null s then Nothing else Just s
    toSinglePiece = id

instance SinglePiece L.Text where
    fromSinglePiece s = if S.null s then Nothing else Just (L.fromChunks [s])
    toSinglePiece = S.concat . L.toChunks

instance SinglePiece Integer where
    fromSinglePiece s =
        case Data.Text.Read.decimal s of
            Right (i, _) -> Just i
            Left _ -> Nothing
    toSinglePiece = S.pack . show

instance SinglePiece Int where
    fromSinglePiece s =
        case Data.Text.Read.decimal s of
            Right (i, _) -> Just i
            Left _ -> Nothing
    toSinglePiece = S.pack . show

instance SinglePiece Int64 where
    fromSinglePiece s =
        case Data.Text.Read.decimal s of
            Right (i, _) -> Just i
            Left _ -> Nothing
    toSinglePiece = S.pack . show


class MultiPiece s where
    fromMultiPiece :: [S.Text] -> Maybe s
    toMultiPiece :: s -> [S.Text]

instance MultiPiece [String] where
    fromMultiPiece = Just . map S.unpack
    toMultiPiece = map S.pack

instance MultiPiece [S.Text] where
    fromMultiPiece = Just
    toMultiPiece = id

instance MultiPiece [L.Text] where
    fromMultiPiece = Just . map (L.fromChunks . return)
    toMultiPiece = map $ S.concat . L.toChunks
