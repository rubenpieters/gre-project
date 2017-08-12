{-# LANGUAGE TemplateHaskell #-}

module GameResult
  where

import Control.Lens

data GameResult = GameResult
  { _winResult :: [Bool]
  , _turnResult :: Int
  }

makeLenses ''GameResult

aggregateGameResult :: GameResult -> MatchResult
aggregateGameResult (GameResult [p1, p2] _) = MatchResult (check p1) (check p2) 1
  where
    check b = if b then 1 else 0

aggregateGameResults :: [GameResult] -> MatchResult
aggregateGameResults l = foldMap aggregateGameResult l

data MatchResult = MatchResult
  { _player1Wins :: Int
  , _player2Wins :: Int
  , _totalGames :: Int
  }

instance Monoid MatchResult where
  mempty = MatchResult 0 0 0
  mappend (MatchResult a1 b1 c1) (MatchResult a2 b2 c2) = MatchResult (a1 + a2) (b1 + b2) (c1 + c2)

makeLenses ''MatchResult
