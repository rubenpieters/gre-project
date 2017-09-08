{-# LANGUAGE TemplateHaskell #-}

module Deck
  where

import Control.Lens

import Card
import CardEffect

data Deck = Deck
  { _frontL :: [Card]
  , _frontM :: [Card]
  , _frontR :: [Card]
  , _backL :: [Card]
  , _backM :: [Card]
  , _backR :: [Card]
  }

makeLenses ''Deck

type DeckPos = (DeckRow, DeckColumn)

drawDeck :: Deck -> String
drawDeck d =
  "+-++-++-+\n"
  ++ "|" ++ show (length $ _frontL d)
  ++ "||" ++ show (length $ _frontM d)
  ++ "||" ++ show (length $ _frontR d) ++ "|\n"
  ++ "+-++-++-+\n"
  ++ "+-++-++-+\n"
  ++ "|" ++ show (length $ _backL d)
  ++ "||" ++ show (length $ _backM d)
  ++ "||" ++ show (length $ _backR d) ++ "|\n"
  ++ "+-++-++-+"
