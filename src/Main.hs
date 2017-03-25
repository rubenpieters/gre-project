{-# LANGUAGE TemplateHaskell #-}

--module Main where

import Control.Lens

data Card =
  NumberCard Int
  | BuffCard Location Int
  | NullCard
  deriving (Show, Eq)

buff f (NumberCard x) = NumberCard (f x)
buff _ c@BuffCard{} = c
buff _ c@NullCard = c

data Location = Board | Hand | Deck deriving (Show, Eq)

type Board = [Card]
type Hand = [Card]
type Deck = [Card]

data PlayerState = PlayerState
  { _hand :: Hand
  , _deck :: Deck
  , _board :: Board
  }
  deriving (Eq)

instance Show PlayerState where
  show PlayerState {_hand = h, _deck = [], _board = b} =
    "PlayerState {hand: " ++ show h ++ ", deck <empty>, board: " ++ show b ++ "}"
  show PlayerState {_hand = h, _deck = (c:_), _board = b} =
    "PlayerState {hand: " ++ show h ++ ", deck (next card): " ++ show c ++ ", board: " ++ show b ++ "}"

makeLenses ''PlayerState

mkPlayer d = PlayerState {_hand = [], _deck = d, _board = replicate 7 NullCard}

locAsLens Board = board
locAsLens Hand = hand
locAsLens Deck = hand

data GameState = GameState
  { _playerState :: [PlayerState]
  , _playerTurn :: Int
  }
  deriving (Show)

makeLenses ''GameState

type PlayerTurn = Int
type TurnBound = Int

playCard :: PlayerTurn -> Card -> GameState -> GameState
playCard pt c@NumberCard{} gameState =
  gameState & (playerState . element pt . board . element 0) .~ c
playCard pt (BuffCard loc amount) gameState =
  gameState & (playerState . element pt . locAsLens loc . traverse) %~ buff (+ amount)
playCard _ NullCard gameState = gameState

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

advanceGame :: TurnBound -> GameState -> GameState
advanceGame bound gameState = nextGameState & playerTurn %~ advancePlayerTurn bound where
  nextGameState = executeTurn (_playerTurn gameState) gameState

deck1 = BuffCard Board 1 : NumberCard 1 : deck1

player1 = mkPlayer deck1
player2 = mkPlayer deck1

gs = GameState {_playerState = [player1, player2], _playerTurn = 0}

turns = iterate (advanceGame 1) gs

main :: IO ()
main = do
  putStrLn "hello world"
