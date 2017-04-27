{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib
  ( Card(..)
  , Location(..)
  , Target(..)
  , CardModType(..)
  , PlayerState(..)
  , hand
  , deck
  , board
  , winner
  , testGame2
  ) where

import Control.Lens

import System.Random.Shuffle
import Control.Monad.Random

data Location = Board | Hand | Deck deriving (Show, Eq)

data Target = All | Friendly | Enemy deriving (Show, Eq)

data CardModType = Add Int | Min Int | Mod Int deriving (Show, Eq)

data CardCappingType = NoCap | MaxCap Int | MinCap Int deriving (Show, Eq)

data CardProps = CardProps CardModType CardCappingType deriving (Show, Eq)

data Card =
  NumberCard Int
  | BuffCard Location Target CardProps
  | CheckAndBuffCard Location Target
  | NullCard
  deriving (Show, Eq)

type Board = [Card]
type Hand = [Card]
type Deck = [Card]

data PlayerState = PlayerState
  { _hand :: Hand
  , _deck :: Deck
  , _board :: Board
  , _winner :: Bool
  }
  deriving (Eq)

instance Show PlayerState where
  show PlayerState {_hand = h, _deck = [], _board = b, _winner = w} =
    "PlayerState {hand: " ++ show h ++ ", deck <empty>, board: " ++ show b ++ ", winner: " ++ show w ++ "}"
  show PlayerState {_hand = h, _deck = (c:_), _board = b, _winner = w} =
    "PlayerState {hand: " ++ show h ++ ", deck (next card): " ++ show c ++ ", board: " ++ show b ++ ", winner: " ++ show w ++ "}"

makeLenses ''PlayerState

mkPlayer d = PlayerState {_hand = [], _deck = d, _board = replicate 7 NullCard, _winner = False}


locAsLens Board = board
locAsLens Hand = hand
locAsLens Deck = hand

tgtAsLens pt All = traverse
tgtAsLens pt Friendly = element pt
tgtAsLens pt Enemy = element (1 - pt)

cmtAsFunc :: CardModType -> Int -> Int
cmtAsFunc (Add x) = (+) x
cmtAsFunc (Min x) = (-) x
cmtAsFunc (Mod x) = mod x

cctAsFunc :: CardCappingType -> Int -> Int
cctAsFunc NoCap x = x
cctAsFunc (MaxCap cap) x = if x > cap then cap else x
cctAsFunc (MinCap cap) x = if x < cap then cap else x

cardPropFunc (CardProps cmt cct) c = buff (cctAsFunc cct . cmtAsFunc cmt) c

buff f c = case ncFunc (NumberCard . f) c of
  Just c' -> c'
  Nothing -> c

cardVal c = ncFunc id c

ncFunc f (NumberCard x) = Just (f x)
ncFunc _ _ = Nothing

data GameState = GameState
  { _playerState :: [PlayerState]
  , _playerTurn :: Int
  , _turnCount :: Int
  }
  deriving (Show)

makeLenses ''GameState

type PlayerTurn = Int
type TurnBound = Int

playCard :: PlayerTurn -> Card -> GameState -> GameState
playCard pt c@NumberCard{} gameState =
  gameState & (playerState . element pt . board . element (lowestCardIndex playerBoard)) .~ c where
  playerBoard = gameState ^. (playerState . element pt . board)
  --toReplaceSpot = playerBoard & (element (lowestCardIndex playerBoard))
playCard pt (BuffCard loc tgt cardProps) gameState =
  gameState & (playerState . tgtAsLens pt tgt . locAsLens loc . traverse) %~ cardPropFunc cardProps
playCard _ NullCard gameState = gameState
playCard pt c@(CheckAndBuffCard loc tgt) gameState =
  gameState
  where check = gameState ^.. (playerState . tgtAsLens pt tgt . locAsLens loc . traverse)
        avgCardVal = traverse cardVal check

-- boards are technically not supposed to be empty
-- but it still gives us index `0` if we would pass it an empty list as board
lowestCardIndex b = go b NullCard 0 0 where
  go [] _ accI _ = accI
  go (c:b') NullCard accI currI = go b' c accI (currI + 1)
  go (c@(NullCard):b') _ _ currI = go b' c currI (currI + 1)
  go (c@(NumberCard x):b') (NumberCard y) _ currI | x < y = go b' c currI (currI + 1)
  go (_:b') acc accI currI = go b' acc accI (currI + 1)

drawCard :: PlayerState -> PlayerState
drawCard ps@PlayerState {_deck = [], _hand = _} = ps
-- we can prepend this if the order within hand doesn't matter
-- currently playing cards is based on the first element, so it does
drawCard ps@PlayerState {_deck = (c:d), _hand = h} = ps & deck .~ d & hand .~ h ++ [c]

discardCard :: PlayerState -> PlayerState
discardCard ps@PlayerState {_hand = []} = ps
discardCard ps@PlayerState {_hand = (_:h)} = ps & hand .~ h

executeTurn :: PlayerTurn -> GameState -> GameState
executeTurn pt gameState = afterDiscardCard where
  afterDrawCard = gameState & (playerState . element pt) %~ drawCard
  maybeCardToPlay :: Maybe Card
  maybeCardToPlay = afterDrawCard ^? (playerState . element pt . hand . element 0)
  play :: Maybe Card -> GameState -> GameState
  play (Just c) = playCard pt c
  play Nothing = id
  afterPlayCard = play maybeCardToPlay afterDrawCard
  afterDiscardCard = afterPlayCard & (playerState . element pt) %~ discardCard

advancePlayerTurn :: TurnBound -> PlayerTurn -> PlayerTurn
advancePlayerTurn bound current =
  if candidate > bound
  then 0
  else candidate
  where
  candidate = current + 1

checkWinCon :: PlayerState -> PlayerState
checkWinCon ps@PlayerState {_board = b} =
  if all cardWinCon b
  then ps & winner .~ True
  else ps
  where
  cardWinCon (NumberCard 100) = True
  cardWinCon _ = False

advanceGame :: TurnBound -> GameState -> GameState
advanceGame bound gameState = if any _winner (_playerState gameState)
  then gameState
  else afterCheckWinner
  where
  nextGameState = executeTurn (_playerTurn gameState) gameState
  afterAdvanceTurn = nextGameState & playerTurn %~ advancePlayerTurn bound
  afterAdvanceTurnCount = afterAdvanceTurn & turnCount %~ (+) 1
  afterCheckWinner = afterAdvanceTurnCount & (playerState . traverse) %~ checkWinCon

--deck1 = NumberCard 1 : replicate 100 (BuffCard Board 1) ++ deck1
deck1 = NumberCard 0 : BuffCard Board Friendly (CardProps (Add 5) (MaxCap 100)) : deck1
deck2 = (replicate 1000 (NumberCard 5)) ++ (replicate 10000 (BuffCard Board Friendly (CardProps (Add 5) (MaxCap 100))))
deck3 = [NumberCard 0]

randomPermutations :: MonadRandom m => [a] -> m [a]
randomPermutations l = do
  x <- shuffleM l
  y <- randomPermutations l
  return (x ++ y)

player1 = mkPlayer deck3
player2 = mkPlayer deck3

testGame = GameState {_playerState = [player1, player2], _playerTurn = 0, _turnCount = 0}

testGame2 :: MonadRandom m => m (Int, [Bool])
testGame2 = do
  deckP1 <- shuffleM deck2
  deckP2 <- shuffleM deck2
  let states = playFunc deckP1 deckP2
  let lastState = last states
  let nextState = advanceGame 1 lastState
  return (nextState ^. turnCount, nextState ^.. playerState . traverse . winner)

turns = iterate (advanceGame 1) testGame

playUntilWinner = takeWhile (\gs -> none _winner (gs ^. playerState)) turns

playFunc :: [Card] -> [Card] -> [GameState]
playFunc d1 d2 = takeWhile playCond allStates where
  pl1 = mkPlayer d1
  pl2 = mkPlayer d2
  allStates = iterate (advanceGame 1) GameState {_playerState = [pl1, pl2], _playerTurn = 0, _turnCount = 0}

playCond :: GameState -> Bool
playCond gs = none _winner (gs ^. playerState) && (gs ^. turnCount) < 1000

main :: IO ()
main = do
  putStrLn "hello world"
