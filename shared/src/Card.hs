{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Card
  where

import Control.Monad.Operational
import Control.Lens

type CardId = Int

data DeckColumn = L | M | R
  deriving (Show, Eq, Ord)

data DeckRow = F | B
  deriving (Show, Eq, Ord)

type CardEffect = Program CardEffectOp ()

data CardEffectOp a where
  QueueDmg :: Int -> Int -> DeckColumn -> CardEffectOp ()
  AddDP :: Int -> CardEffectOp ()
  AddCard :: Card -> DeckColumn -> CardEffectOp ()

data Card = Card
  { _cardId :: CardId
  , _cardEffect :: CardEffect
  }

makeLenses ''Card

dmgCard :: Card
dmgCard = Card
  { _cardId = 1
  , _cardEffect = singleton (AddDP 1)
  }

focusCard :: Card
focusCard = Card
  { _cardId = 2
  , _cardEffect = return ()
  }

drawCard :: Card -> String
drawCard c | _cardId c == 1 = "D"
drawCard c | _cardId c == 2 = "F"
drawCard c = show $ _cardId c
