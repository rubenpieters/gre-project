{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Player
  where

import Data.Maybe

import Control.Lens hiding (view)
import Control.Lens.Cons
import Control.Monad.Operational

import Card
import Deck
import Hand
import Timer

data Player = Player
  { _deck :: Deck
  , _hand :: Hand
  , _timersL :: [Timer]
  , _timersM :: [Timer]
  , _timersR :: [Timer]
  , _dps :: Int
  }

makeLenses ''Player

drawPlayer :: Player -> String
drawPlayer p = drawDeck pDeck
  where
    pDeck = _deck p

colToTimer :: DeckColumn -> Lens' Player [Timer]
colToTimer L = timersL
colToTimer M = timersM
colToTimer R = timersR

colToFront :: DeckColumn -> Lens' Player [Card]
colToFront L = deck . frontL
colToFront M = deck . frontM
colToFront R = deck . frontR

type HandIx = Int

testDeck = Deck
  { _frontL = [dmgCard, dmgCard]
  , _frontM = [dmgCard, dmgCard]
  , _frontR = [dmgCard, dmgCard]
  , _backL = [dmgCard, dmgCard]
  , _backM = [dmgCard, dmgCard]
  , _backR = [dmgCard, dmgCard]
  }

testPlayer' = Player
  { _deck = testDeck
  , _hand = []
  , _timersL = []
  , _timersM = []
  , _timersR = []
  , _dps = 0
}

dummyDeck = Deck
  { _frontL = []
  , _frontM = []
  , _frontR = []
  , _backL = []
  , _backM = []
  , _backR = []
  }
dummyPlayer = Player
  { _deck = dummyDeck
  , _hand = []
  , _timersL = []
  , _timersM = []
  , _timersR = []
  , _dps = 0
}

drawPhase :: Player -> Player
drawPhase p = p6 & hand %~ (catMaybes l' ++)
  where
    (c1, p1) = setAndReturn p ht (deck . frontL)
    (c2, p2) = setAndReturn p1 ht (deck . frontM)
    (c3, p3) = setAndReturn p2 ht (deck . frontR)
    (c4, p4) = setAndReturn p3 ht (deck . backL)
    (c5, p5) = setAndReturn p4 ht (deck . backM)
    (c6, p6) = setAndReturn p5 ht (deck . backR)
    l = zip [c1, c2, c3, c4, c5, c6] [(F, L), (F, M), (F, R), (B, L), (B, M), (B, R)]
    l' = seqTM <$> l

wardPhase :: Player -> Player
wardPhase p = p & deck . frontL %~ (++ cards (F, L))
                & deck . frontM %~ (++ cards (F, M))
                & deck . frontR %~ (++ cards (F, R))
                & deck . backL %~ (++ cards (B, L))
                & deck . backM %~ (++ cards (B, M))
                & deck . backR %~ (++ cards (B, R))
  where
    cards loc = fst <$> filter ((== loc) . snd) (p ^. hand)

seqTM :: (Maybe a, b) -> Maybe (a, b)
seqTM (Just a, b) = Just (a, b)
seqTM (Nothing, _) = Nothing

setAndReturn :: s -> (x -> (t, x)) -> Lens' s x -> (t, s)
setAndReturn a f l = (x, a & l .~ y)
  where (x, y) = f (a ^. l)

ht :: [a] -> (Maybe a, [a])
ht [] = (Nothing, [])
ht l = (Just $ head l, tail l)
