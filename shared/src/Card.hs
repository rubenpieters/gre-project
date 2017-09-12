{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Card
  where

import Control.Monad
import Control.Monad.Operational
import Control.Lens

type CardId = Int

data DeckColumn = L | M | R
  deriving (Show, Eq, Ord)

data DeckRow = F | B
  deriving (Show, Eq, Ord)

data Origin = Origin
  { _column :: DeckColumn
  , _player :: Int
  }

makeLenses ''Origin

type CardEffect = Program CardEffectOp ()

data CardEffectOp a where
  AddDP :: Int -> CardEffectOp ()
  AddCard :: Card -> DeckColumn -> CardEffectOp ()
  Log :: String -> CardEffectOp ()
  AddTimer :: Int -> Bool -> CardEffect -> CardEffectOp ()
  GetOrigin :: CardEffectOp Origin
  Blocked :: CardEffectOp Bool

addDP a = singleton $ AddDP a
addCard a b = singleton $ AddCard a b
logG a = singleton $ Log a
addTimer a b c = singleton $ AddTimer a b c
getOrigin = singleton GetOrigin
blocked = singleton Blocked

data Card = Card
  { _cardId :: CardId
  , _cardEffect :: CardEffect
  }

makeLenses ''Card

dmgCard :: Card
dmgCard = Card
  { _cardId = 1
  , _cardEffect = do
      logG "activate dmg card"
      addDP 1
  }

focusCard :: Card
focusCard = Card
  { _cardId = 2
  , _cardEffect =
      logG "activate focus card"
  }

dmg11Card :: Card
dmg11Card = Card
  { _cardId = 3
  , _cardEffect = do
      logG "queue 1 dmg timer 2"
      addTimer 2 False $ do
        b <- blocked
        if b
          then
            logG "(blocked) dmg 1"
          else do
            o <- getOrigin
            logG $ "dmg 1 " ++ show (o ^. column)
            replicateM_ 1 (addCard dmgCard (o ^. column))
  }

blockCard :: Card
blockCard = Card
  { _cardId = 4
  , _cardEffect = do
    logG "refresh block 2"
    addTimer 2 True $ return ()
  }

drawCard :: Card -> String
drawCard c | _cardId c == 1 = "D"
drawCard c | _cardId c == 2 = "F"
drawCard c = show $ _cardId c
