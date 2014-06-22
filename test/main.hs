{-# Language ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Test.Hspec
import Test.Hspec.QuickCheck(prop)
import Test.QuickCheck

import Web.PathPieces
import qualified Data.Text as T
import Data.Maybe (fromJust)

-- import FileLocation (debug)

instance Arbitrary T.Text where
  arbitrary = fmap T.pack arbitrary


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "PathPiece" $ do
    prop "toPathPiece <=> fromPathPiece String" $ \(p::String) ->
      case (fromPathPiece . toPathPiece) p of
        Nothing -> null p
        Just pConverted -> p == pConverted

    prop "toPathPiece <=> fromPathPiece Text" $ \(p::T.Text) ->
      case (fromPathPiece . toPathPiece) p of
        Nothing -> T.null p
        Just pConverted -> p == pConverted

    prop "toPathPiece <=> fromPathPiece Int" $ \(p::Int) ->
      case (fromPathPiece . toPathPiece) p of
        Nothing -> False
        Just pConverted -> p == pConverted

    prop "toPathPiece <=> fromPathPiece Bool" $ \(p::Bool) ->
      case (fromPathPiece . toPathPiece) p of
        Nothing -> False
        Just pConverted -> p == pConverted

    prop "toPathPiece <=> fromPathPiece String" $ \(p::Maybe String) ->
      case (fromPathPiece . toPathPiece) p of
        Nothing -> False
        Just pConverted -> p == pConverted

  describe "PathMultiPiece" $ do
    prop "toPathMultiPiece <=> fromPathMultiPiece String" $ \(p::[String]) ->
      p == (fromJust . fromPathMultiPiece . toPathMultiPiece) p

    prop "toPathMultiPiece <=> fromPathMultiPiece Text" $ \(p::[T.Text]) ->
      p == (fromJust . fromPathMultiPiece . toPathMultiPiece) p


  describe "SinglePiece" $ do
    prop "toPathPiece <=> fromPathPiece String" $ \(p::String) ->
      case (fromPathPiece . toPathPiece) p of
        Nothing -> null p
        Just pConverted -> p == pConverted

    prop "toPathPiece <=> fromPathPiece Text" $ \(p::T.Text) ->
      case (fromPathPiece . toPathPiece) p of
        Nothing -> T.null p
        Just pConverted -> p == pConverted

    prop "toPathPiece <=> fromPathPiece Int" $ \(p::Int) ->
      case (fromPathPiece . toPathPiece) p of
        Nothing -> p < 0
        Just pConverted -> p == pConverted

  describe "MultiPiece" $ do
    prop "toPathMultiPiece <=> fromPathMultiPiece String" $ \(p::[String]) ->
      p == (fromJust . fromPathMultiPiece . toPathMultiPiece) p

    prop "toPathMultiPiece <=> fromPathMultiPiece Text" $ \(p::[T.Text]) ->
      p == (fromJust . fromPathMultiPiece . toPathMultiPiece) p
