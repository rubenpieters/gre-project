{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module GameState
  where

import Control.Lens hiding (view)
import Control.Monad.Operational

import Card
import Player
import Timer

data GameState = GameState
  { _player1 :: Player
  , _player2 :: Player
  , _turn :: Int
  }

makeLenses ''GameState

drawGameState :: GameState -> String
drawGameState gs =
  "player1\n" ++
  "player2\n"

data Target = Opp | Self
  deriving (Show, Eq, Ord)

target :: Int -> Target -> Lens' GameState Player
target 1 Self = player1
target 1 Opp = player2
target 2 Self = player2
target 2 Opp = player1
target _ _ = error "invalid turn value"

target' :: GameState -> Target -> Lens' GameState Player
target' gs = target (_turn gs)

evalCE :: Program CardEffectOp a -> GameState -> (GameState, a)
evalCE = eval . view
  where
    eval :: ProgramView CardEffectOp a -> GameState -> (GameState, a)
    eval (Return x) gs = (gs, x)
    eval (QueueDmg dmg cd col :>>= k) gs =
      evalCE (k ()) (gs & target' gs Self . colToTimer col %~ (timerDmg dmg cd col :))
    eval (AddDP x :>>= k) gs =
      evalCE (k ()) (gs & target' gs Self . dps %~ (x +))
    eval (AddCard c col :>>= k) gs =
      evalCE (k ()) (gs & target' gs Opp . colToFront col %~ (++ [c]))

execEff :: Card -> GameState -> GameState
execEff c gs = fst $ evalCE (_cardEffect c) gs

runTurn :: GameState -> GameState
runTurn gs = gsAfterWard
  where
    gsAfterDraw = gs & player1 %~ drawPhase
                     & player2 %~ drawPhase
    cardP1 = fst $ head $ gsAfterDraw ^. player1 . hand
    cardP2 = fst $ head $ gsAfterDraw ^. player2 . hand
    gsAfterEff = execEff cardP2 (execEff cardP1 gsAfterDraw)
    gsAfterWard = gsAfterEff & player1 %~ wardPhase
                             & player2 %~ wardPhase
