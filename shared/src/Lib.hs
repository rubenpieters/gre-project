{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

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

import Derive.Het

import Control.Lens
import Control.Applicative
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

data CardProps = CardProps CardModType CardCappingType RangeType
  deriving (Generic, Show, Eq, Ord)

data RangeType = NoRange | RangeMinPlus Int Int
  deriving (Generic, Show, Eq, Ord)

data CardFilter = NoFilter | Lowest
  deriving (Generic, Show, Eq, Ord)

type PlayerTurn = Int
type TurnBound = Int
type CardPointer = (PlayerTurn, Location, Int)

data BoardCheckType a where
  AvgCardVal :: BoardCheckType Int
  HighestCardIx :: BoardCheckType Int
  LowestCardIx :: BoardCheckType Int
  LowestHighestIx :: BoardCheckType (Int, Int)
  HighestCardPtr :: BoardCheckType CardPointer

deriving instance Show (BoardCheckType a)

genHetCmp ''BoardCheckType
genHetEq ''BoardCheckType
genHetLte ''BoardCheckType

data BoardTransformType a where
  SetAll :: BoardTransformType Int
  DoIf :: BoardTransformType Bool
  CopyLR :: BoardTransformType Int
  Absorb :: BoardTransformType (Int, Int)
  BuffIx :: CardProps -> BoardTransformType Int
  BuffPtr :: CardProps -> BoardTransformType CardPointer

deriving instance Show (BoardTransformType a)

genHetCmp ''BoardTransformType
genHetEq ''BoardTransformType
genHetLte ''BoardTransformType

data CheckTransform where
  CheckTransform :: BoardCheckType a -> BoardTransformType a -> CheckTransform

deriving instance Show CheckTransform

instance Eq CheckTransform where
  (CheckTransform bct1 btt1) == (CheckTransform bct2 btt2) = heqBoardCheckType bct1 bct2 && heqBoardTransformType btt1 btt2

instance Ord CheckTransform where
  (CheckTransform bct1 btt1) <= (CheckTransform bct2 btt2) = hlteBoardCheckType bct1 bct2 && hlteBoardTransformType btt1 btt2

data Card =
  NumberCard Int
  | BuffCard Location Target CardFilter CardProps
  | CheckAndBuffCard Location Target
  | CheckTransformCard Location Target CheckTransform
  | Move2ToTop Card Card
  | ComposedCard Card Card
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

rtAsFunc :: (MonadRandom m) => RangeType -> Int -> m Int
rtAsFunc NoRange x = return x
rtAsFunc (RangeMinPlus min plus) x = do
  bias <- uniform [min .. plus]
  return $ x + bias

cmtAsFunc :: (MonadRandom m) => CardModType -> RangeType -> Int -> m Int
cmtAsFunc (Add x) rt y = do z <- rtAsFunc rt x ; return $ y + z
cmtAsFunc (Min x) rt y = do z <- rtAsFunc rt x ; return $ y - z
cmtAsFunc (Mod x) rt y = do z <- rtAsFunc rt x ; return $ y `mod` z
cmtAsFunc (Set x) rt _ = do z <- rtAsFunc rt x ; return $ z

cctAsFunc :: CardCappingType -> Int -> Int
cctAsFunc NoCap x = x
cctAsFunc (MaxCap cap) x = if x > cap then cap else x
cctAsFunc (MinCap cap) x = if x < cap then cap else x

bctAsFunc :: BoardCheckType a -> GameState -> Location -> Target -> PlayerTurn -> Maybe a
bctAsFunc AvgCardVal gs loc _ pt = average <$> traverse cardVal cs
   where cs = allTargets gs loc All pt
bctAsFunc HighestCardIx gs loc tgt pt = Just $ lowestCardIndex (>) cs
   where cs = allTargets gs loc tgt pt
bctAsFunc LowestCardIx gs loc tgt pt = Just $ lowestCardIndex (<) cs
   where cs = allTargets gs loc tgt pt
bctAsFunc LowestHighestIx gs loc tgt pt = (,) <$> bctAsFunc LowestCardIx gs loc tgt pt <*> bctAsFunc HighestCardIx gs loc tgt pt
bctAsFunc HighestCardPtr gs loc _ _ = snd <$> findMaxCard cardInfo
  --_ $ targetsWithIndex gs loc tgt <$.> allPlayers -- highestCardPlayer gs loc tgt <$.> allPlayers
  where
    -- TODO all players should depend on tgt?
    allPlayers = [0..1]
    cardInfo =  do
      pl <- allPlayers
      -- Friendly to target only the cards of this player
      (ix, card) <- targetsWithIndex gs loc Friendly pl
      return (card, (pl, loc, ix))

(<$.>) :: (Functor f) => (a -> b) -> f a -> f (a, b)
(<$.>) f fa = (\a -> (a, f a)) <$> fa

targetsWithIndex :: GameState -> Location -> Target -> PlayerTurn -> [(Int, Card)]
targetsWithIndex gs loc tgt pt = zip [0..] cs
  where cs = allTargets gs loc tgt pt

findMaxCard :: (Eq a) => [(Card, a)] -> Maybe (Card, a)
findMaxCard xs = safeFoldableFunc (maximumBy (\(c1,_) (c2,_) -> compare c1 c2)) xs

highestCardPlayer :: GameState -> Location -> Target -> PlayerTurn -> Maybe (Int, Card)
highestCardPlayer gs loc tgt pt = (safeFoldableFunc $ maximumBy (\(_, mc1) (_, mc2) -> compare mc1 mc2)) csIndexed
  where
    cs = allTargets gs loc tgt pt
    csIndexed = zip [0..] cs

safeFoldableFunc :: (Alternative t, Foldable t, Eq (t a)) => (t a -> b) -> t a -> Maybe b
safeFoldableFunc _ z | z == empty = Nothing
safeFoldableFunc f xs = Just $ f xs

bttAsFunc :: (MonadRandom m) => BoardTransformType a -> a -> Location -> Target -> PlayerTurn -> GameState -> m GameState
bttAsFunc SetAll a loc tgt pt gs = do
  transformCardsM gs loc tgt pt $ \cards -> mapM (buffM $ cmtAsFunc (Set a) NoRange) cards
bttAsFunc CopyLR a loc tgt pt gs = return $ transformCards gs loc tgt pt $ \cards ->
  let value = cards ^? element a in
  case value of
  Just x -> cards & element (a-1) .~ x
                  & element (a+1) .~ x
  Nothing -> cards
bttAsFunc Absorb (l, r) loc tgt pt gs = return $ transformCards gs loc tgt pt $ \cards ->
  let
    mCardL = cards ^? element l
    mCardR = cards ^? element r
    cardsLR = (,) <$> mCardL <*> mCardR
  in
  case cardsLR of
  -- TODO: add capping
    Just (cardL, _) -> cards & element l %~ buff (const 0)
                             & element r %~ buff (maybeNc 0 cardL + )
    Nothing -> cards
bttAsFunc (BuffIx props) a loc tgt pt gs = transformCardsM gs loc tgt pt $ \cards -> mapMOf (element a) (cardPropFunc props) cards
-- tgt is Friendly because we need to target the player `pt`
bttAsFunc (BuffPtr props) (pt, loc, i) _ tgt _ gs = mapMOf (playerState . tgtAsLens pt Friendly . locAsLens loc . element i) (cardPropFunc props) gs
  --gs & (playerState . tgtAsLens pt Friendly . locAsLens loc . element i) %~ cardPropFunc props

--transformCards :: GameState -> Location -> Target -> PlayerTurn -> GameState
transformCards gs loc tgt pt f = gs & (playerState . tgtAsLens pt tgt . locAsLens loc) %~ f

transformCardsM :: (Monad m) => GameState -> Location -> Target -> PlayerTurn -> (Board -> m Board) -> m GameState
transformCardsM gs loc tgt pt f = mapMOf (playerState . tgtAsLens pt tgt . locAsLens loc) f gs

allTargets :: GameState -> Location -> Target -> PlayerTurn -> [Card]
allTargets gs loc tgt pt = gs ^. (playerState . tgtAsLens pt tgt . locAsLens loc)

average :: Foldable t => t Int -> Int
average xs = sum xs `div` length xs

cardPropFunc :: (MonadRandom m) => CardProps -> Card -> m Card
cardPropFunc (CardProps cmt cct rt) c = do
  buffM (liftM (cctAsFunc cct) . cmtAsFunc cmt rt) c

buff :: (Int -> Int) -> Card -> Card
buff f c = case ncFunc (NumberCard . f) c of
  Just c' -> c'
  Nothing -> c

buffM :: (Monad m) => (Int -> m Int) -> Card -> m Card
buffM f c = case ncFunc (liftM NumberCard . f) c of
  Just c' -> c'
  Nothing -> return c

cardVal c = ncFunc id c

maybeNc :: Int -> Card -> Int
maybeNc x = fromMaybe x . ncFunc id

ncFunc f (NumberCard x) = Just (f x)
ncFunc _ _ = Nothing

liftToCard :: (Int -> b) -> (Card -> b)
liftToCard f = f . maybeNc 0

playCard :: (MonadRandom m) => PlayerTurn -> Card -> GameState -> m GameState
playCard pt c@NumberCard{} gameState = return $
  gameState & (playerState . element pt . board . element (lowestCardIndex (<) playerBoard)) .~ c
  where playerBoard = gameState ^. (playerState . element pt . board)
  --toReplaceSpot = playerBoard & (element (lowestCardIndex playerBoard))
playCard pt (BuffCard loc tgt filt cardProps) gameState = 
  mapMOf (playerState . tgtAsLens pt tgt . locAsLens loc . traverse . filtAsLens gameState loc tgt pt filt) (cardPropFunc cardProps) gameState
playCard _ NullCard gameState = return gameState
playCard pt c@(CheckAndBuffCard loc tgt) gameState =
  case avgCardVal of
    Just x -> mapMOf (playerState . tgtAsLens pt tgt . locAsLens loc . traverse)
                (cardPropFunc (CardProps (Set x) NoCap NoRange)) gameState
    Nothing -> return gameState
  where check = gameState ^.. (playerState . tgtAsLens pt tgt . locAsLens loc . traverse)
        avgCardVal = average <$> traverse cardVal check 
playCard pt c@(CheckTransformCard loc tgt (CheckTransform check transform)) gameState =
  transformFunc gameState
  where
    checkValue = bctAsFunc check gameState loc tgt pt
    transformFunc = case checkValue of
      Just a -> bttAsFunc transform a loc tgt pt
      Nothing -> return

-- TODO: handle case c1 == c2 differently
-- TODO: what should happen when only 1 card can be found (currently nothing happens)
playCard pt (Move2ToTop c1 c2) gameState = return $ case newDeck of
  Just newDeck' -> gameState & (playerState . tgtAsLens pt Friendly . locAsLens Deck) .~ newDeck'
  Nothing -> gameState
  where
    deck = gameState ^.. (playerState . tgtAsLens pt Friendly . locAsLens Deck . traverse)
    newDeck = do
      i1 <- findIndexStartingFrom 0 c1 deck
      i2 <- findIndexStartingFrom 0 c2 deck
      return $ [deck !! i1, deck !! i2] ++ exceptIndices deck [i1, i2]
playCard pt (ComposedCard c1 c2) gameState = do
  gs' <- playCard pt c1 gameState
  playCard pt c2 gs'

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

executeTurn :: (MonadWriter [GameLog] m, MonadRandom m) => PlayerTurn -> GameState -> m GameState
executeTurn pt gameState = do
  tell [GameLog ("Player " ++ (show pt) ++ " Playing " ++ (show maybeCardToPlay))]
  afterPlayCard <- play maybeCardToPlay afterDrawCard
  return $ afterDiscardCard afterPlayCard
  where
    afterDrawCard = gameState & (playerState . element pt) %~ drawCard
    maybeCardToPlay :: Maybe Card
    maybeCardToPlay = afterDrawCard ^? (playerState . element pt . hand . element 0)
    play :: (MonadRandom m) => Maybe Card -> GameState -> m GameState
    play (Just c) gs = playCard pt c gs
    play Nothing gs = return gs
    afterDiscardCard gs = gs & (playerState . element pt) %~ discardCard

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

advanceGame :: (MonadWriter [GameLog] m, MonadRandom m) => TurnBound -> GameState -> m GameState
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
deck1 = NumberCard 0 : BuffCard Board Friendly NoFilter (CardProps (Add 5) (MaxCap 100) NoRange) : deck1
deck2 = (replicate 1000 (NumberCard 5)) ++ (replicate 10000 (BuffCard Board Friendly NoFilter (CardProps (Add 5) (MaxCap 100) NoRange)))
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
card2 = BuffCard Board All Lowest (CardProps (Add 5) (MaxCap 100) NoRange)
card3 = BuffCard Board Friendly NoFilter (CardProps (Add 2) (MaxCap 100) NoRange)
card4 = CheckTransformCard Board Friendly (CheckTransform HighestCardIx CopyLR)
card5 = CheckTransformCard Board Friendly (CheckTransform LowestHighestIx Absorb)
card6 = CheckTransformCard Board All (CheckTransform AvgCardVal SetAll)
card7 = BuffCard Deck Friendly NoFilter (CardProps (Add 2) (MaxCap 100) NoRange)
card8 = BuffCard Board All NoFilter (CardProps (Set 0) NoCap NoRange)
-- TODO: this card should remove 1 from all integer-valued cards, not only NumberCards
card9 = BuffCard Deck Enemy NoFilter (CardProps (Min 1) (MinCap 0) NoRange)
card10 = Move2ToTop
card11 = BuffCard Board All NoFilter (CardProps (Add 0) (MaxCap 100) (RangeMinPlus 1 10))
card12 = CheckTransformCard Board Friendly (CheckTransform HighestCardIx (BuffIx (CardProps (Add 5) (MaxCap 100) NoRange)))
card17_addFriendly = BuffCard Board Friendly NoFilter (CardProps (Add 1) (MaxCap 100) NoRange)
card17_dmgEnemy = BuffCard Board Enemy NoFilter (CardProps (Min 1) (MinCap 0) NoRange)
card17 = ComposedCard card17_addFriendly card17_dmgEnemy
card18 = CheckTransformCard Board All (CheckTransform HighestCardPtr (BuffPtr (CardProps (Add 10) (MaxCap 100) NoRange)))

atList :: Int -> Lens' [a] a
atList n = singular (ix n)

swapLens :: (Int -> Lens' fa a) -> Int -> Int -> fa -> fa
swapLens at i j fx = set (at i) (view (at j) fx)
                   $ set (at j) (view (at i) fx)
                   $ fx

main :: IO ()
main = do
  putStrLn "hello world"
