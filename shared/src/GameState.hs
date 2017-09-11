{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module GameState
  where

import Data.Either

import Control.Monad.State
import Control.Monad.Writer
import Control.Lens hiding (view)
import Control.Monad.Operational

import Card
import Player
import Timer
import UserInput

data GameState = GameState
  { _player1 :: Player
  , _player2 :: Player
  , _turn :: Int
  }

makeLenses ''GameState

testGS :: GameState
testGS = GameState
  { _player1 = testPlayer'
  , _player2 = dummyPlayer
  , _turn = 1
  }

drawGameState :: GameState -> String
drawGameState gs =
  "player1\n"
  ++ drawPlayer (_player1 gs) ++ "\n"
  ++ "player2\n"
  ++ drawPlayer (_player2 gs) ++ "\n"

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

evalCE :: (MonadState GameState m, MonadWriter [String] m)
       => Program CardEffectOp a -> Origin -> m a
evalCE p o = flip eval o $ view p
  where
    eval :: (MonadState GameState m, MonadWriter [String] m)
         => ProgramView CardEffectOp a -> Origin -> m a
    eval (Return x) o = return x
    eval (Log s :>>= k) o = do
      tell [s]
      evalCE (k ()) o
    eval (AddDP x :>>= k) o = do
      gs <- get
      target' gs Self . dps %= (x +)
      evalCE (k ()) o
    eval (AddCard c col :>>= k) o = do
      gs <- get
      target' gs Opp . colToFront col %= (++ [c])
      evalCE (k ()) o
    eval (AddTimer cd p' :>>= k) o = do
      gs <- get
      target' gs Self . colToTimer L %= ((Timer cd p') :)
      evalCE (k ()) o
    eval (GetOrigin :>>= k) o = evalCE (k o) o

execEff :: (MonadWriter [String] m) => CardEffect -> Origin -> GameState -> m GameState
execEff ce o gs = execStateT (evalCE ce o) gs

execCard :: (MonadWriter [String] m) => Card -> Origin -> GameState -> m GameState
execCard c o gs = execEff (_cardEffect c) o gs

execEffs :: (MonadWriter [String] m) => [CardEffect] -> Origin -> GameState -> m GameState
execEffs [] _ gs = return gs
execEffs (c:cs) o gs = execEff c o gs >>= execEffs cs o

tickPhase :: (MonadWriter [String] m) => GameState -> m GameState
tickPhase gs = do
  gs7 <- execEffs ces1 (Origin L 1) gs6
  gs8 <- execEffs ces2 (Origin M 1) gs7
  gs9 <- execEffs ces3 (Origin R 1) gs8
  gs10 <- execEffs ces4 (Origin L 2) gs9
  gs11 <- execEffs ces5 (Origin M 2) gs10
  gs12 <- execEffs ces6 (Origin R 2) gs11
  return gs12
  where
    (ces1, gs1) = gs & (player1 . timersL) %%~ newTimersAndEffs
    (ces2, gs2) = gs1 & (player1 . timersM) %%~ newTimersAndEffs
    (ces3, gs3) = gs2 & (player1 . timersR) %%~ newTimersAndEffs
    (ces4, gs4) = gs3 & (player2 . timersL) %%~ newTimersAndEffs
    (ces5, gs5) = gs4 & (player2 . timersM) %%~ newTimersAndEffs
    (ces6, gs6) = gs5 & (player2 . timersR) %%~ newTimersAndEffs

newTimersAndEffs :: [Timer] -> ([CardEffect], [Timer])
newTimersAndEffs ts = partitionEithers $ tick <$> ts

runTurn :: (MonadWriter [String] m, MonadIO m)
        => GameState -> m GameState
runTurn gs = do
  gsAfterTick <- tickPhase gs
  let gsAfterDraw = gsAfterTick & player1 %~ drawPhase
                                & player2 %~ drawPhase
  iP1 <- evalUiCli $ playHand $ fst <$> gsAfterDraw ^. player1 . hand
  let (cardP1, (row1, col1)) =
        (gsAfterDraw ^. player1 . hand) !! iP1
  let (cardP2, (row2, col2)) =
        head $ gsAfterDraw ^. player2 . hand
  gsAfterEff1 <- execCard cardP1 (Origin col1 1) gsAfterDraw
  gsAfterEff2 <- execCard cardP2 (Origin col2 2) gsAfterEff1
  return $ gsAfterEff2 & player1 %~ wardPhase
                       & player2 %~ wardPhase

runTurn' :: GameState -> IO (GameState, [String])
runTurn' = runWriterT . runTurn

runTurn'' :: GameState -> IO GameState
runTurn'' gs = do
  (newGs, log) <- runWriterT $ runTurn gs
  mapM_ putStrLn log
  return newGs

runTurnX :: Int -> GameState -> IO GameState
runTurnX 0 gs = do
  putStrLn $ drawGameState gs
  return gs
runTurnX i gs = do
  putStrLn $ "----- " ++ show i ++ "\n"
  putStrLn $ drawGameState gs
  newGS <- runTurn'' gs
  runTurnX (i-1) newGS
