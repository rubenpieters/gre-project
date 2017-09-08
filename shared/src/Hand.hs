{-# LANGUAGE TemplateHaskell #-}

module Hand
  where

import Data.List

import Control.Lens

import Card
import Deck

type Hand = [(Card, DeckPos)]

drawHand :: Hand -> String
drawHand h = intercalate "\n" $
  foldr (\c [t, m, b] ->
  [t ++ "+-+", m ++ "|" ++ drawCard c ++ "|", b ++ "+-+"]
  ) ["", "", ""] (fst <$> h)
