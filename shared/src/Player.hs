{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Player
  where

import Data.Maybe
import Data.List

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
  , _actions :: Int
  }

makeLenses ''Player

drawPlayer :: Player -> String
drawPlayer p =
  drawDeck (_deck p)
  ++ "\n"
  ++ "timers:\n"
  ++ "L:" ++ intercalate "|" (drawTimer <$> _timersL p)
  ++ "\n"
  ++ "M:" ++ intercalate "|" (drawTimer <$> _timersM p)
  ++ "\n"
  ++ "R:" ++ intercalate "|" (drawTimer <$> _timersR p)
  ++ "\n"
  ++ "DPS:" ++ show (_dps p)

colToTimer :: DeckColumn -> Lens' Player [Timer]
colToTimer L = timersL
colToTimer M = timersM
colToTimer R = timersR

colToFront :: DeckColumn -> Lens' Player [Card]
colToFront L = deck . frontL
colToFront M = deck . frontM
colToFront R = deck . frontR

posToPile :: DeckPos -> Lens' Player [Card]
posToPile (F, L) = deck . frontL
posToPile (F, M) = deck . frontM
posToPile (F, R) = deck . frontR
posToPile (B, L) = deck . backL
posToPile (B, M) = deck . backM
posToPile (B, R) = deck . backR

originToPile :: Origin -> Lens' Player [Card]
originToPile (Origin c r _) = posToPile (r,c)

type HandIx = Int

testDeck = Deck
  { _frontL = [block2Card]
  , _frontM = [block2Card]
  , _frontR = [block2Card]
  , _backL = [incCard 1]
  , _backM = [incCard 1]
  , _backR = [incCard 1]
  }

testPlayer' = Player
  { _deck = testDeck
  , _hand = []
  , _timersL = []
  , _timersM = []
  , _timersR = []
  , _dps = 0
  , _actions = 0
}

dummyDeck = Deck
  { _frontL = [dmg11Card]
  , _frontM = [blockCard]
  , _frontR = [blockCard]
  , _backL = [blockCard]
  , _backM = [blockCard]
  , _backR = [blockCard]
  }
dummyPlayer = Player
  { _deck = dummyDeck
  , _hand = []
  , _timersL = []
  , _timersM = []
  , _timersR = []
  , _dps = 0
  , _actions = 0
}

initPhase :: Player -> Player
initPhase p = p & actions .~ 1

drawPos :: DeckPos -> Player -> Player
drawPos pos p = case mc of
  Just c -> p' & hand %~ ((c, pos) :)
  Nothing -> p
  where
    (mc, p') = p & posToPile pos %%~ ht

drawPhase :: Player -> Player
drawPhase p = p6 & hand %~ (l' ++)
  where
    (c1, p1) = p & deck . frontL %%~ ht
    (c2, p2) = p1 & deck . frontM %%~ ht
    (c3, p3) = p2 & deck . frontR %%~ ht
    (c4, p4) = p3 & deck . backL %%~ ht
    (c5, p5) = p4 & deck . backM %%~ ht
    (c6, p6) = p5 & deck . backR %%~ ht
    l = zip [c1, c2, c3, c4, c5, c6] [(F, L), (F, M), (F, R), (B, L), (B, M), (B, R)]
    l' = l ^@.. traverse . itraversed . indexMaybe id

indexMaybe :: Indexable j p => Applicative f => (i -> Maybe j) -> Optical' p (Indexed i) f a a
indexMaybe p f = Indexed $ \i a -> maybe (pure a) (indexed f ?? a) (p i)

--test :: [(String, String)]
--test = [(Just "a", "b"), (Nothing, ""), (Just "c", "d")] ^.. traverse . _ _1 . _Just

wardPhase :: Player -> Player
wardPhase p = p & hand .~ []
                & deck . frontL %~ (++ cards (F, L))
                & deck . frontM %~ (++ cards (F, M))
                & deck . frontR %~ (++ cards (F, R))
                & deck . backL %~ (++ cards (B, L))
                & deck . backM %~ (++ cards (B, M))
                & deck . backR %~ (++ cards (B, R))
  where
    cards loc = p ^.. hand . traverse . filtered (\x -> x ^. _2 == loc) . _1

ht :: [a] -> (Maybe a, [a])
ht [] = (Nothing, [])
ht l = (Just $ head l, tail l)


data GameState = GameState
  { _player1 :: Player
  , _player2 :: Player
  , _turn :: Int
  }

makeLenses ''GameState

