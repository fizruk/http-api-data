{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Web.PathPieces
    ( SinglePiece (..)
    , MultiPiece (..)
    ) where

import Data.Int (Int64)
import qualified Data.Text as S
import qualified Data.Text.Lazy as L
import qualified Data.Text.Read

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
