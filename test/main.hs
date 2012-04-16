{-# Language ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Test.Hspec.Monadic (Specs, describe, hspecX)
import Test.Hspec.HUnit()
import Test.Hspec.QuickCheck(prop)
import Test.QuickCheck

import Web.PathPieces
import qualified Data.Text as T
import Data.Maybe (fromJust)

-- import FileLocation (debug)

instance Arbitrary T.Text where
  arbitrary = fmap T.pack arbitrary


main :: IO ()
main = hspecX specs

specs :: Specs
specs = do
  describe "PathPiece" $ do
    prop "toPathPiece <=> fromSinglePiece String" $ \(p::String) ->
      case (fromPathPiece . toSinglePiece) p of
        Nothing -> null p
        Just pConverted -> p == pConverted

    prop "toPathPiece <=> fromSinglePiece String" $ \(p::T.Text) ->
      case (fromPathPiece . toSinglePiece) p of
        Nothing -> T.null p
        Just pConverted -> p == pConverted

    prop "toPathPiece <=> fromSinglePiece String" $ \(p::Int) ->
      case (fromPathPiece . toSinglePiece) p of
        Nothing -> p < 0
        Just pConverted -> p == pConverted

  describe "PathMultiPiece" $ do
    prop "toPathMultiPiece <=> fromMultiPiece String" $ \(p::[String]) ->
      p == (fromJust . fromPathMultiPiece . toMultiPiece) p

    prop "toPathMultiPiece <=> fromMultiPiece String" $ \(p::[T.Text]) ->
      p == (fromJust . fromPathMultiPiece . toMultiPiece) p


  describe "SinglePiece" $ do
    prop "toSinglePiece <=> fromSinglePiece String" $ \(p::String) ->
      case (fromSinglePiece . toSinglePiece) p of
        Nothing -> null p
        Just pConverted -> p == pConverted

    prop "toSinglePiece <=> fromSinglePiece String" $ \(p::T.Text) ->
      case (fromSinglePiece . toSinglePiece) p of
        Nothing -> T.null p
        Just pConverted -> p == pConverted

    prop "toSinglePiece <=> fromSinglePiece String" $ \(p::Int) ->
      case (fromSinglePiece . toSinglePiece) p of
        Nothing -> p < 0
        Just pConverted -> p == pConverted

  describe "MultiPiece" $ do
    prop "toMultiPiece <=> fromMultiPiece String" $ \(p::[String]) ->
      p == (fromJust . fromMultiPiece . toMultiPiece) p

    prop "toMultiPiece <=> fromMultiPiece String" $ \(p::[T.Text]) ->
      p == (fromJust . fromMultiPiece . toMultiPiece) p
