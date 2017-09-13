{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module GameState
  where

import Data.Either
import Data.Maybe

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Lens hiding (view)
import Control.Monad.Operational

import Card
import Player
import Hand
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

target' :: Origin -> Target -> Lens' GameState Player
target' o = target (o ^. player)

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
      target' o Self . dps %= (x +)
      evalCE (k ()) o
    eval (AddCard c col :>>= k) o = do
      target' o Opp . colToFront col %= (++ [c])
      evalCE (k ()) o
    eval (AddTimer cd b p' :>>= k) o = do
      fb <- use (target' o Self . colToTimer (o ^. column) . to firstBlocking)
      if b && isJust fb
        then -- merge blocking timers
          let (i, Timer cd2 p2 _) = fromJust fb
          in target' o Self . colToTimer (o ^. column) . ix i .= Timer (max cd cd2) (p' >> p2) True
        else -- add timer
          target' o Self . colToTimer (o ^. column) %= (Timer cd p' b :)
      evalCE (k ()) o
    eval (GetOrigin :>>= k) o = evalCE (k o) o
    eval (Blocked :>>= k) o = do
      fb <- use (target' o Opp . colToTimer (o ^. column) . to firstBlocking)
      maybe
        (return ())
        (\(i, _) -> target' o Opp . colToTimer (o ^. column) %= deleteNth i)
        fb
      evalCE (k (isJust fb)) o

deleteNth i xs = take i xs ++ drop (succ i) xs

evalCR  :: (MonadReader GameState m)
       => Program CardReqOp a -> Origin -> m a
evalCR p o = flip eval o $ view p
  where
    eval :: (MonadReader GameState m)
         => ProgramView CardReqOp a -> Origin -> m a
    eval (Return x) o = return x
    eval (FocusCards :>>= k) o = do
      gs <- ask
      let (Sum focus) = gs ^. target' o Self . hand . traverse . filtered (isFocus . fst) . to (const $ Sum 1)
      evalCR (k focus) o
    eval (ComboCards cid :>>= k) o = do
      gs <- ask
      let (Sum combo) = gs ^. target' o Self . hand . traverse . filtered (isCombo cid . fst) . to (const $ Sum 1)
      evalCR (k combo) o

cardsOnlyReq :: GameState -> Origin -> Hand -> Hand
cardsOnlyReq gs o h = hWithReq ^.. traverse . filtered (\x -> x ^. _2) . _1
  where
    hWithReq = (\c@(x, _) -> (c, runReader (evalCR (x ^. cardReqs) o) gs)) <$> h

execCard :: (MonadState GameState m, MonadWriter [String] m)
         => Card -> Origin -> m ()
execCard c o = evalCE (_cardEffect c) o

execEffs :: (MonadState GameState m, MonadWriter [String] m)
         => [CardEffect] -> Origin -> m ()
execEffs [] _ = return ()
execEffs (c:cs) o = evalCE c o >> execEffs cs o

tickPhase :: (MonadState GameState m, MonadWriter [String] m)
          => m ()
tickPhase = do
  ces1 <- (player1 . timersL) %%= newTimersAndEffs
  ces2 <- (player1 . timersM) %%= newTimersAndEffs
  ces3 <- (player1 . timersR) %%= newTimersAndEffs
  ces4 <- (player2 . timersL) %%= newTimersAndEffs
  ces5 <- (player2 . timersM) %%= newTimersAndEffs
  ces6 <- (player2 . timersR) %%= newTimersAndEffs
  execEffs ces1 (Origin L 1)
  execEffs ces2 (Origin M 1)
  execEffs ces3 (Origin R 1)
  execEffs ces4 (Origin L 2)
  execEffs ces5 (Origin M 2)
  execEffs ces6 (Origin R 2)

newTimersAndEffs :: [Timer] -> ([CardEffect], [Timer])
newTimersAndEffs ts = partitionEithers $ tick <$> ts

runTurn :: (MonadState GameState m, MonadWriter [String] m, MonadIO m)
        => m ()
runTurn = do
  player1 %= initPhase
  player2 %= initPhase
  tickPhase
  player1 %= drawPhase
  player2 %= drawPhase
  gsAfterDraw <- get
  let fullHand1 = gsAfterDraw ^.. player1 . hand . traverse
  --fullHand1 <- use (player1 . hand)
  let filteredHand1 = cardsOnlyReq gsAfterDraw (Origin undefined 1) fullHand1
  i1 <- evalUiCli $ playHand (fst <$> fullHand1) (fst <$> filteredHand1)
  let (cardP1, (row1, col1)) =
       filteredHand1 !! i1
  let (cardP2, (row2, col2)) =
        head $ gsAfterDraw ^. player2 . hand
  execCard cardP1 (Origin col1 1)
  execCard cardP2 (Origin col2 2)
  player1 %= wardPhase
  player2 %= wardPhase

--runTurn' :: GameState -> IO (GameState, [String])
--runTurn' gs = runWriterT . execStateT runTurn gs

runTurn'' :: GameState -> IO GameState
runTurn'' gs = do
  (newGs, log) <- runWriterT $ execStateT runTurn gs
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
