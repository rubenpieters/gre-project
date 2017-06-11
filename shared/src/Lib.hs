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

import Data.List
import Data.Maybe
import Data.Foldable

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

data CardFilter = NoFilter | Lowest
  deriving (Generic, Show, Eq, Ord)

data BoardCheckType a where
  AvgCardVal :: BoardCheckType Int
  HighestCardIx :: BoardCheckType Int
  LowestCardIx :: BoardCheckType Int
  LowestHighestIx :: BoardCheckType (Int, Int)

instance Show (BoardCheckType a) where
  show AvgCardVal = "AvgCardVal"
  show HighestCardIx = "HighestCardIx"
  show LowestCardIx = "LowestCardIx"
  show LowestHighestIx = "LowestHighestIx"

bcEq :: BoardCheckType a -> BoardCheckType b -> Bool
bcEq AvgCardVal AvgCardVal = True
bcEq HighestCardIx HighestCardIx = True
bcEq LowestCardIx LowestCardIx = True
bcEq LowestHighestIx LowestHighestIx = True
bcEq _ _ = False

bcLte :: BoardCheckType a -> BoardCheckType b -> Bool
bcLte a b | bcEq a b = True
bcLte AvgCardVal HighestCardIx = True
bcLte AvgCardVal LowestHighestIx = True
bcLte AvgCardVal LowestCardIx = True
bcLte HighestCardIx LowestHighestIx = True
bcLte HighestCardIx LowestCardIx = True
bcLte LowestCardIx LowestHighestIx = True
bcLte _ _ = False

data BoardTransformType a where
  SetAll :: BoardTransformType Int
  DoIf :: BoardTransformType Bool
  CopyLR :: BoardTransformType Int
  Absorb :: BoardTransformType (Int, Int)
  BuffIx :: CardProps -> BoardTransformType Int

instance Show (BoardTransformType a) where
  show SetAll = "SetAll"
  show DoIf = "DoIf"
  show CopyLR = "CopyLR"
  show Absorb = "Absorb"
  show (BuffIx props) = "BuffIx " ++ show props

btEq :: BoardTransformType a -> BoardTransformType b -> Bool
btEq SetAll SetAll = True
btEq DoIf DoIf = True
btEq CopyLR CopyLR = True
btEq Absorb Absorb = True
btEq (BuffIx p1) (BuffIx p2) | p1 == p2 = True
btEq _ _ = False

btLte :: BoardTransformType a -> BoardTransformType b -> Bool
btLte (BuffIx p1) (BuffIx p2) | p1 <= p2 = True
btLte a b | btEq a b = True
btLte SetAll DoIf = True
btLte SetAll CopyLR = True
btLte SetAll Absorb = True
btLte SetAll BuffIx{} = True
btLte DoIf CopyLR = True
btLte DoIf Absorb = True
btLte DoIf BuffIx{} = True
btLte CopyLR Absorb = True
btLte CopyLR BuffIx{} = True
btLte Absorb BuffIx{} = True
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
  | BuffCard Location Target CardFilter CardProps
  | CheckAndBuffCard Location Target
  | CheckTransformCard Location Target CheckTransform
  | Move2ToTop Card Card
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

filtAsLens _ _ _ _ NoFilter = filtered (const True)
filtAsLens gs loc tgt pt Lowest = filtered (liftToCard lowestFilter)
  where
    targets = allTargets gs loc tgt pt
    lowestCardValue = foldl (\cm c -> min cm (maybeNc 0 c)) 999999 targets
    lowestFilter = (==) lowestCardValue

cmtAsFunc :: CardModType -> Int -> Int
cmtAsFunc (Add x) = (+) x
cmtAsFunc (Min x) = \z -> z - x
cmtAsFunc (Mod x) = mod x
cmtAsFunc (Set x) = const x

cctAsFunc :: CardCappingType -> Int -> Int
cctAsFunc NoCap x = x
cctAsFunc (MaxCap cap) x = if x > cap then cap else x
cctAsFunc (MinCap cap) x = if x < cap then cap else x

bctAsFunc :: BoardCheckType a -> [Card] -> Maybe a
bctAsFunc AvgCardVal cs = average <$> traverse cardVal cs
bctAsFunc HighestCardIx cs = Just $ lowestCardIndex (>) cs
bctAsFunc LowestCardIx cs = Just $ lowestCardIndex (<) cs
bctAsFunc LowestHighestIx cs = (,) <$> bctAsFunc LowestCardIx cs <*> bctAsFunc HighestCardIx cs

bttAsFunc :: BoardTransformType a -> a -> [Card] -> [Card]
bttAsFunc SetAll a cards = map (buff $ cmtAsFunc (Set a)) cards
bttAsFunc CopyLR a cards = case value of
  Just x -> cards & element (a-1) .~ x
                  & element (a+1) .~ x
  Nothing -> cards
  where
    value = cards ^? element a
bttAsFunc Absorb (l, r) cards = case cardsLR of
  -- TODO: add capping
  Just (cardL, _) -> cards & element l %~ buff (const 0)
                           & element r %~ buff (maybeNc 0 cardL + )
  Nothing -> cards
  where
    mCardL = cards ^? element l
    mCardR = cards ^? element r
    cardsLR = (,) <$> mCardL <*> mCardR
bttAsFunc (BuffIx props) a cards = cards & element a %~ cardPropFunc props

allTargets :: GameState -> Location -> Target -> PlayerTurn -> [Card]
allTargets gs loc tgt pt = gs ^. (playerState . tgtAsLens pt tgt . locAsLens loc)

average :: Foldable t => t Int -> Int
average xs = sum xs `div` length xs

cardPropFunc :: CardProps -> Card -> Card
cardPropFunc (CardProps cmt cct) c = buff (cctAsFunc cct . cmtAsFunc cmt) c

buff f c = case ncFunc (NumberCard . f) c of
  Just c' -> c'
  Nothing -> c

cardVal c = ncFunc id c

maybeNc :: Int -> Card -> Int
maybeNc x = fromMaybe x . ncFunc id

ncFunc f (NumberCard x) = Just (f x)
ncFunc _ _ = Nothing

liftToCard :: (Int -> b) -> (Card -> b)
liftToCard f = f . maybeNc 0

playCard :: PlayerTurn -> Card -> GameState -> GameState
playCard pt c@NumberCard{} gameState =
  gameState & (playerState . element pt . board . element (lowestCardIndex (<) playerBoard)) .~ c
  where playerBoard = gameState ^. (playerState . element pt . board)
  --toReplaceSpot = playerBoard & (element (lowestCardIndex playerBoard))
playCard pt (BuffCard loc tgt filt cardProps) gameState =
  gameState & (playerState . tgtAsLens pt tgt . locAsLens loc . traverse . filtAsLens gameState loc tgt pt filt) %~ cardPropFunc cardProps
playCard _ NullCard gameState = gameState
playCard pt c@(CheckAndBuffCard loc tgt) gameState =
  case avgCardVal of
    Just x -> gameState & (playerState . tgtAsLens pt tgt . locAsLens loc . traverse)
                %~ cardPropFunc (CardProps (Set x) NoCap)
    Nothing -> gameState
  where check = gameState ^.. (playerState . tgtAsLens pt tgt . locAsLens loc . traverse)
        avgCardVal = average <$> traverse cardVal check
playCard pt c@(CheckTransformCard loc tgt (CheckTransform check transform)) gameState =
  gameState & (playerState . tgtAsLens pt tgt . locAsLens loc) %~ transformFunc
  where
    checkTargets = allTargets gameState loc tgt pt
    checkValue = bctAsFunc check checkTargets
    transformFunc = case checkValue of
      Just a -> bttAsFunc transform a
      Nothing -> id
-- TODO: handle case c1 == c2 differently
-- TODO: what should happen when only 1 card can be found (currently nothing happens)
playCard pt (Move2ToTop c1 c2) gameState = case newDeck of
  Just newDeck' -> gameState & (playerState . tgtAsLens pt Friendly . locAsLens Deck) .~ newDeck'
  Nothing -> gameState
  where
    deck = gameState ^.. (playerState . tgtAsLens pt Friendly . locAsLens Deck . traverse)
    newDeck = do
      i1 <- findIndexStartingFrom 0 c1 deck
      i2 <- findIndexStartingFrom 0 c2 deck
      return $ [deck !! i1, deck !! i2] ++ exceptIndices deck [i1, i2]

findIndexStartingFrom :: (Eq a) => Int -> a -> [a] -> Maybe Int
findIndexStartingFrom start a l = findIndex (a ==) (drop start l)

exceptIndex :: (Foldable f) => f a -> Int -> [a]
exceptIndex l i = l ^.. (folded . ifiltered (\i' _ -> i' /= i))

exceptIndices :: (Foldable f) => f a -> [Int] -> [a]
exceptIndices l i = l ^.. (folded . ifiltered (\i' _ -> not $ elem i' i))



-- boards are technically not supposed to be empty
-- but it still gives us index `0` if we would pass it an empty list as board
--
-- currently returns first element if multiple targets
-- TODO: make it return all and select one element with a criterium
lowestCardIndex :: (Num a) => (Int -> Int -> Bool) -> [Card] -> a
lowestCardIndex f b = go b NullCard 0 0 where
  go [] _ accI _ = accI
  go (c:b') NullCard accI currI = go b' c accI (currI + 1)
  go (c@(NullCard):b') _ _ currI = go b' c currI (currI + 1)
  go (c@(NumberCard x):b') (NumberCard y) _ currI | f x y = go b' c currI (currI + 1)
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
deck1 = NumberCard 0 : BuffCard Board Friendly NoFilter (CardProps (Add 5) (MaxCap 100)) : deck1
deck2 = (replicate 1000 (NumberCard 5)) ++ (replicate 10000 (BuffCard Board Friendly NoFilter (CardProps (Add 5) (MaxCap 100))))
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

testPlayer2 = PlayerState {_hand = [], _deck = [NumberCard 1, NumberCard 2, NumberCard 3, NumberCard 4], _board = [NumberCard 1, NumberCard 2, NumberCard 10], _winner = False}
testBoard2 = GameState {_playerState = [testPlayer2, testPlayer], _playerTurn = 0, _turnCount = 0}


card1 = NumberCard 10
card2 = BuffCard Board All Lowest (CardProps (Add 5) (MaxCap 100))
card3 = BuffCard Board Friendly NoFilter (CardProps (Add 2) (MaxCap 100))
card4 = CheckTransformCard Board Friendly (CheckTransform HighestCardIx CopyLR)
card5 = CheckTransformCard Board Friendly (CheckTransform LowestHighestIx Absorb)
card6 = CheckTransformCard Board All (CheckTransform AvgCardVal SetAll)
card7 = BuffCard Deck Friendly NoFilter (CardProps (Add 2) (MaxCap 100))
card8 = BuffCard Board All NoFilter (CardProps (Set 0) NoCap)
-- TODO: this card should remove 1 from all integer-valued cards, not only NumberCards
card9 = BuffCard Deck Enemy NoFilter (CardProps (Min 1) (MinCap 0))
card10 = Move2ToTop
card12 = CheckTransformCard Board Friendly (CheckTransform HighestCardIx (BuffIx (CardProps (Add 5) (MaxCap 100))))
card18 = CheckTransformCard Board All (CheckTransform HighestCardIx (BuffIx (CardProps (Add 10) (MaxCap 100))))

atList :: Int -> Lens' [a] a
atList n = singular (ix n)

swapLens :: (Int -> Lens' fa a) -> Int -> Int -> fa -> fa
swapLens at i j fx = set (at i) (view (at j) fx)
                   $ set (at j) (view (at i) fx)
                   $ fx

main :: IO ()
main = do
  putStrLn "hello world"
