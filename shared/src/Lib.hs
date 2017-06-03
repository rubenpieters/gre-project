{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Lib
{-}  ( GameLog
  , Card(..)
  , Location(..)
  , Target(..)
  , CardModType(..)
  , CardCappingType(..)
  , CardProps(..)
  , PlayerState(..)
  , locAsLens
  , hand
  , deck
  , deck2
  , board
  , winner
  , testGame2
  , testGame'
  , discardCard
  , playXTimes
  , runGameIO
  , BoardCheckType(..)
  , BoardTransformType(..)
  , CheckTransform(..)
  )-}
  where

import System.Random.Shuffle

import Control.Lens
import Control.Monad
import Control.Monad.Loops
import Control.Monad.Writer (MonadWriter(tell),runWriterT)
import Control.Monad.Random

import GHC.Generics

data GameLog = GameLog String
  deriving (Generic, Show)

data Location = Board | Hand | Deck
  deriving (Generic, Show, Eq, Ord)

data Target = All | Friendly | Enemy
  deriving (Generic, Show, Eq, Ord)

data CardModType = Add Int | Min Int | Mod Int | Set Int
  deriving (Generic, Show, Eq, Ord)

data CardCappingType = NoCap | MaxCap Int | MinCap Int
  deriving (Generic, Show, Eq, Ord)

data CardProps = CardProps CardModType CardCappingType
  deriving (Generic, Show, Eq, Ord)

data BoardCheckType a where
  AvgCardVal :: BoardCheckType Int

instance Show (BoardCheckType a) where
  show AvgCardVal = "AvgCardVal"

bcEq :: BoardCheckType a -> BoardCheckType b -> Bool
bcEq AvgCardVal AvgCardVal = True
bcEq _ _ = False

bcLte :: BoardCheckType a -> BoardCheckType b -> Bool
bcLte AvgCardVal AvgCardVal = True
bcLte _ _ = False

data BoardTransformType a where
  SetAll :: BoardTransformType Int
  DoIf :: BoardTransformType Bool

instance Show (BoardTransformType a) where
  show SetAll = "SetAll"
  show DoIf = "DoIf"

btEq :: BoardTransformType a -> BoardTransformType b -> Bool
btEq SetAll SetAll = True
btEq DoIf DoIf = True
btEq _ _ = False

btLte :: BoardTransformType a -> BoardTransformType b -> Bool
btLte SetAll SetAll = True
btLte SetAll DoIf = True
btLte DoIf DoIf = True
btLte _ _ = False

data CheckTransform where
  CheckTransform :: BoardCheckType a -> BoardTransformType a -> CheckTransform

instance Show CheckTransform where
  show (CheckTransform bct btt) = "CheckTransform " ++ show bct ++ " " ++ show btt

instance Eq CheckTransform where
  (CheckTransform bct1 btt1) == (CheckTransform bct2 btt2) = bcEq bct1 bct2 && btEq btt1 btt2

instance Ord CheckTransform where
  (CheckTransform bct1 btt1) <= (CheckTransform bct2 btt2) = bcLte bct1 bct2 && btLte btt1 btt2


data Card =
  NumberCard Int
  | BuffCard Location Target CardProps
  | CheckAndBuffCard Location Target
  | CheckTransformCard Location Target CheckTransform
  | NullCard
  deriving (Generic, Show, Eq, Ord)

type Board = [Card]
type Hand = [Card]
type Deck = [Card]

data PlayerState = PlayerState
  { _hand :: Hand
  , _deck :: Deck
  , _board :: Board
  , _winner :: Bool
  }
  deriving (Generic, Eq)

instance Show PlayerState where
  show PlayerState {_hand = h, _deck = [], _board = b, _winner = w} =
    "PlayerState {hand: " ++ show h ++ ", deck <empty>, board: " ++ show b ++ ", winner: " ++ show w ++ "}"
  show PlayerState {_hand = h, _deck = (c:_), _board = b, _winner = w} =
    "PlayerState {hand: " ++ show h ++ ", deck (next card): " ++ show c ++ ", board: " ++ show b ++ ", winner: " ++ show w ++ "}"

makeLenses ''PlayerState

mkPlayer d = PlayerState {_hand = [], _deck = d, _board = replicate 7 NullCard, _winner = False}

type PlayerTurn = Int
type TurnBound = Int

data GameState = GameState
  { _playerState :: [PlayerState]
  , _playerTurn :: Int
  , _turnCount :: Int
  }
  deriving (Eq, Show)

makeLenses ''GameState

locAsLens Board = board
locAsLens Hand = hand
locAsLens Deck = deck

tgtAsLens pt All = traverse
tgtAsLens pt Friendly = element pt
tgtAsLens pt Enemy = element (1 - pt)

cmtAsFunc :: CardModType -> Int -> Int
cmtAsFunc (Add x) = (+) x
cmtAsFunc (Min x) = (-) x
cmtAsFunc (Mod x) = mod x
cmtAsFunc (Set x) = const x

cctAsFunc :: CardCappingType -> Int -> Int
cctAsFunc NoCap x = x
cctAsFunc (MaxCap cap) x = if x > cap then cap else x
cctAsFunc (MinCap cap) x = if x < cap then cap else x

bctAsFunc :: BoardCheckType a -> [Card] -> Maybe a
bctAsFunc AvgCardVal cs = fmap average $ traverse cardVal cs

bttAsFunc :: BoardTransformType a -> a -> [Card] -> [Card]
bttAsFunc SetAll a cards = map (buff $ cmtAsFunc (Set a)) cards

allTargets :: GameState -> Location -> Target -> PlayerTurn -> [Card]
allTargets gs loc tgt pt = gs ^. (playerState . tgtAsLens pt tgt . locAsLens loc)

average :: Foldable t => t Int -> Int
average xs = sum xs `div` length xs

cardPropFunc (CardProps cmt cct) c = buff (cctAsFunc cct . cmtAsFunc cmt) c

buff f c = case ncFunc (NumberCard . f) c of
  Just c' -> c'
  Nothing -> c

cardVal c = ncFunc id c

ncFunc f (NumberCard x) = Just (f x)
ncFunc _ _ = Nothing

playCard :: PlayerTurn -> Card -> GameState -> GameState
playCard pt c@NumberCard{} gameState =
  gameState & (playerState . element pt . board . element (lowestCardIndex playerBoard)) .~ c
  where playerBoard = gameState ^. (playerState . element pt . board)
  --toReplaceSpot = playerBoard & (element (lowestCardIndex playerBoard))
playCard pt (BuffCard loc tgt cardProps) gameState =
  gameState & (playerState . tgtAsLens pt tgt . locAsLens loc . traverse) %~ cardPropFunc cardProps
playCard _ NullCard gameState = gameState
playCard pt c@(CheckAndBuffCard loc tgt) gameState =
  case avgCardVal of
    Just x -> gameState & (playerState . tgtAsLens pt tgt . locAsLens loc . traverse)
                %~ cardPropFunc (CardProps (Set x) NoCap)
    Nothing -> gameState
  where check = gameState ^.. (playerState . tgtAsLens pt tgt . locAsLens loc . traverse)
        avgCardVal = fmap average $ traverse cardVal check
playCard pt c@(CheckTransformCard loc tgt (CheckTransform check transform)) gameState =
  gameState & (playerState . tgtAsLens pt tgt . locAsLens loc) %~ transformFunc
  where
    checkTargets = allTargets gameState loc tgt pt
    checkValue = bctAsFunc check checkTargets
    transformFunc = case checkValue of
      Just a -> bttAsFunc transform a
      Nothing -> id

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

executeTurn :: (MonadWriter [GameLog] m) => PlayerTurn -> GameState -> m GameState
executeTurn pt gameState = do
  tell [GameLog ("Player " ++ (show pt) ++ " Playing " ++ (show maybeCardToPlay))]
  return afterDiscardCard
  where
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

advanceGame :: (MonadWriter [GameLog] m) => TurnBound -> GameState -> m GameState
advanceGame bound gameState = do
  nextGameState <- executeTurn (_playerTurn gameState) gameState
  return $ advanceTurn bound nextGameState

advanceTurn :: TurnBound -> GameState -> GameState
advanceTurn bound gameState = if any _winner (_playerState gameState)
  then gameState
  else afterCheckWinner
  where
    afterAdvanceTurn = gameState & playerTurn %~ advancePlayerTurn bound
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

testGame2 :: (MonadWriter [GameLog] m, MonadRandom m) => m (Int, [Bool])
testGame2 = do
  deckP1 <- shuffleM deck2
  deckP2 <- shuffleM deck2
  let lastState = playFunc deckP1 deckP2
--  states' <- states
--  let lastState = last states'
  nextState <- lastState >>= advanceGame 1
  return (nextState ^. turnCount, nextState ^.. playerState . traverse . winner)

testGame' :: (MonadWriter [GameLog] m, MonadRandom m) => [Card] -> [Card] -> m (Int, [Bool])
testGame' deckP1' deckP2' = do
  tell [GameLog "--- Starting Game ---"]
  deckP1 <- shuffleM deckP1'
  deckP2 <- shuffleM deckP2'
  let lastState = playFunc deckP1 deckP2
--  states' <- states
--  let lastState = last states'
  nextState <- lastState >>= advanceGame 1
  return (nextState ^. turnCount, nextState ^.. playerState . traverse . winner)

playXTimes :: (MonadWriter [GameLog] m, MonadRandom m) => [Card] -> [Card] -> Int -> m (Int, Int)
playXTimes deckP1 deckP2 times = fmap (foldr winnerCounting (0, 0)) replicated
  where
    replicated = replicateM times (testGame' deckP1 deckP2)
    winnerCounting (_, [p1, p2]) (x, y) = (x + add p1, y + add p2)
    winnerCounting _ _ = error "less/more than 2 players not supported yet"
    add b = if b then 1 else 0

--turns :: (MonadWriter [GameLog] m, MonadRandom m) => m [GameState]
--turns = iterateM (advanceGame 1) testGame

playUntilWinner :: (MonadWriter [GameLog] m, MonadRandom m) => m GameState
playUntilWinner = do
  iterateUntilM (\gs -> any _winner (gs ^. playerState)) (advanceGame 1) testGame

playFunc :: (MonadWriter [GameLog] m, MonadRandom m) => [Card] -> [Card] -> m GameState
playFunc d1 d2 = do
  allStates
  where
    pl1 = mkPlayer d1
    pl2 = mkPlayer d2
    allStates = iterateUntilM stopCond (advanceGame 1) GameState {_playerState = [pl1, pl2], _playerTurn = 0, _turnCount = 0}

playCond :: GameState -> Bool
playCond gs = none _winner (gs ^. playerState) && (gs ^. turnCount) < 1000

stopCond :: GameState -> Bool
stopCond = not . playCond

--runGameIO :: (MonadWriter w m, MonadRandom m) => m a -> IO (a, w)
runGameIO = evalRandIO . runWriterT


testPlayer = PlayerState {_hand = [], _deck = [], _board = [NumberCard 1, NumberCard 2, NumberCard 3], _winner = False}
testBoard = GameState {_playerState = [testPlayer, testPlayer], _playerTurn = 0, _turnCount = 0}

card3 = BuffCard Board Friendly (CardProps (Add 2) (MaxCap 100))
card8 = BuffCard Board All (CardProps (Set 0) NoCap)

main :: IO ()
main = do
  putStrLn "hello world"
