{-# LANGUAGE FlexibleInstances, TypeSynonymInstances  #-}
module Web.PathPieces
    ( RoutePiece (..)
    , RouteMultiPiece (..)
    -- * Deprecated
    , toSinglePiece
    , toMultiPiece
    , fromSinglePiece
    , fromMultiPiece
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

{-# DEPRECATED toSinglePiece "Use toRoutePiece instead of toSinglePiece" #-}
toSinglePiece :: RoutePiece p => p -> S.Text
toSinglePiece = toRoutePiece

{-# DEPRECATED fromSinglePiece "Use fromRoutePiece instead of fromSinglePiece" #-}
fromSinglePiece :: RoutePiece p => S.Text -> Maybe p
fromSinglePiece = fromRoutePiece

{-# DEPRECATED toMultiPiece "Use toRouteMultiPiece instead of toMultiPiece" #-}
toMultiPiece :: RouteMultiPiece ps => ps -> [S.Text]
toMultiPiece = toRouteMultiPiece

{-# DEPRECATED fromMultiPiece "Use fromRouteMultiPiece instead of fromMultiPiece" #-}
fromMultiPiece :: RouteMultiPiece ps => [S.Text] -> Maybe ps
fromMultiPiece = fromRouteMultiPiece
