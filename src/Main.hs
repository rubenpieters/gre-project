module Main where

import Control.Lens

data Card =
  NumberCard Int
  | BuffCard Location Int

data Location = Board | Hand | Deck -- | Mult Location Location

type Board = [Card]
type Hand = [Card]
type Deck = [Card]

data PlayerState = PlayerState
  { hand :: Hand
  , deck :: Deck
  , board :: Board
  }

type GameState = [PlayerState]

type PlayerTurn = Int

playCard :: PlayerTurn -> Card -> GameState -> GameState
playCard pt c@(NumberCard nc) gameState = set (element pt) newPlayerState gameState where
  playerState = gameState !! pt
  newPlayerState = set (element 0) c (board playerState)

main :: IO ()
main = do
  putStrLn "hello world"
