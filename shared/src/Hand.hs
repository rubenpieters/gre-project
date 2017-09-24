{-# LANGUAGE TemplateHaskell #-}

module Hand
  where

import Data.List

import Control.Lens

import Card
import Deck

type Hand = [(Card, DeckPos)]

drawCards :: [Card] -> String
drawCards cs = intercalate "\n" $
  foldr (\c [t, m, b] ->
  [t ++ "+--+", m ++ "|" ++ drawCard c ++ "|", b ++ "+--+"]
  ) ["", "", ""] cs

drawHand :: Hand -> String
drawHand h = drawCards (fst <$> h)
