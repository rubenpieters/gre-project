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
import Deck
import Timer
import UserInput

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

target :: Int -> Target -> Lens' GameState Player
target 1 Self = player1
target 1 Opp = player2
target 2 Self = player2
target 2 Opp = player1
target _ _ = error "invalid turn value"

target' :: Origin -> Target -> Lens' GameState Player
target' o = target (o ^. player)

evalCE :: (MonadState GameState m, MonadWriter [String] m, MonadIO m)
       => Program CardEffectOp a -> Origin -> m a
evalCE p o = flip eval o $ view p
  where
    eval :: (MonadState GameState m, MonadWriter [String] m, MonadIO m)
         => ProgramView CardEffectOp a -> Origin -> m a
    eval (Return x) o = return x
    eval (Log s :>>= k) o = do
--      tell [s]
      liftIO $ putStrLn s
      evalCE (k ()) o
    eval (AddDP x :>>= k) o = do
      target' o Self . dps %= (x +)
      evalCE (k ()) o
    eval (AddAP x :>>= k) o = do
      target' o Self . actions %= (x +)
      evalCE (k ()) o
    eval (AddCard c ogn tgt :>>= k) o = do
      target' ogn tgt . originToPile ogn %= (++ [c])
      evalCE (k ()) o
    eval (AddTimer cd b p' :>>= k) o = do
      fb <- use (target' o Self . colToTimer (o ^. column) . to firstBlocking)
      if b && isJust fb && (fromJust fb ^. _2 . timerOrigin == o)
        then -- merge blocking timers
          let (i, Timer cd2 _ _ p2) = fromJust fb
          in target' o Self . colToTimer (o ^. column) . ix i .= Timer (max cd cd2) True o (p' >> p2)
        else do -- add timer
          target' o Self . colToTimer (o ^. column) %= (Timer cd b o p':)
      evalCE (k ()) o
    eval (GetOrigin :>>= k) o = evalCE (k o) o
    eval (Blocked :>>= k) o = do
      fb <- use (target' o Opp . colToTimer (o ^. column) . to firstBlocking)
      maybe
        (return ())
        (\(i, _) -> target' o Opp . colToTimer (o ^. column) %= deleteNth i)
        fb
      evalCE (k (isJust fb)) o
    eval (Combo cid :>>= k) o = do
      gs <- get
      let cards = gs ^.. target' o Self . hand . traverse
      let cardsIxs = zip [0..] cards
      let comboCardsIxs = cardsIxs ^.. traverse . filtered (isCombo cid . fst . snd) . _1
      mapM_ (`playCard` o) comboCardsIxs
      evalCE (k ()) o
    eval (Draw o@(Origin c r _) :>>= k) _ = do
      target' o Self %= drawPos (r,c)
      evalCE (k ()) o

deleteNth i xs = take i xs ++ drop (succ i) xs

takeNth i xs = (xs ^? ix i, deleteNth i xs)

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

cardsOnly :: GameState -> Origin -> Hand -> (Card -> Bool) -> [(Card, DeckPos, Int)]
cardsOnly gs o h f = h'Ix'Req ^.. traverse . filtered (\x -> x ^. _2) . _1
  where
    h'Ix = zip h [0..]
    h'Ix'Req = (\(c@(x, y), i) -> ((x, y, i), f x)) <$> h'Ix

cardSatReq gs o x = runReader (evalCR (x ^. cardReqs) o) gs

playCard :: (MonadState GameState m, MonadWriter [String] m, MonadIO m)
         => Int -> Origin -> m ()
playCard i o = do
  playedCard <- (target' o Self . hand) %%= takeNth i
  maybe
    (return ())
    (\(c, _) -> evalCE (c ^. cardEffect) o)
    playedCard

execEffs :: (MonadState GameState m, MonadWriter [String] m, MonadIO m)
         => [CardEffect] -> [Origin] -> m ()
execEffs [] _ = return ()
execEffs (c:cs) (o:os) = evalCE c o >> execEffs cs os

tickPhase :: (MonadState GameState m, MonadWriter [String] m, MonadIO m)
          => m ()
tickPhase = do
  ces1 <- (player1 . timersL) %%= newTimersAndEffs
  ces2 <- (player1 . timersM) %%= newTimersAndEffs
  ces3 <- (player1 . timersR) %%= newTimersAndEffs
  ces4 <- (player2 . timersL) %%= newTimersAndEffs
  ces5 <- (player2 . timersM) %%= newTimersAndEffs
  ces6 <- (player2 . timersR) %%= newTimersAndEffs
  uncurry execEffs $ unzip ces1
  uncurry execEffs $ unzip ces2
  uncurry execEffs $ unzip ces3
  uncurry execEffs $ unzip ces4
  uncurry execEffs $ unzip ces5
  uncurry execEffs $ unzip ces6

newTimersAndEffs :: [Timer] -> ([(CardEffect, Origin)], [Timer])
newTimersAndEffs ts = partitionEithers $ tick <$> ts

damagePhase :: (MonadState GameState m, MonadWriter [String] m, MonadIO m)
            => UIHandler m -> Lens' GameState Player -> Int -> m ()
damagePhase evalUi player pNo = do
  gs <- get
  hand <- use (player . hand)
  when (any (^. _1 . to isDamage) hand)
    (do
      clr <- evalUi clearDmg
      when clr
        (do
          let dCards = cardsOnly gs (Origin undefined undefined pNo) hand isDamage ^.. traverse . to _23
          mapM_ (uncurry $ flip playCard) dCards
          return ()
        )
      damagePhase evalUi player pNo
    )
  where
    bti True = 1
    bti False = 0
    _23 (_,(c,r),b) = (Origin r c pNo,b)

actionPhase :: (MonadState GameState m, MonadWriter [String] m, MonadIO m)
            => UIHandler m -> Lens' GameState Player -> Int -> m ()
actionPhase evalUi player pNo = do
  gs <- get
  fullHand1 <- use (player . hand)
  let filterO = Origin undefined undefined pNo
  let filteredHand1 = cardsOnly gs filterO fullHand1 (cardSatReq gs filterO)
  if (not (null filteredHand1))
    then
    do
      -- ask for action
      i1 <- evalUi $ playHand ((\(a,_,b) -> (a,b)) <$> filteredHand1)
      -- pick chosen card
      let (cardP1, (row1, col1)) = fullHand1 !! i1
      -- reduce player actions
      player . actions %= (\x -> x - 1)
      -- play card
      playCard i1 (Origin col1 row1 pNo)
      -- if any actions left, redo action phase
      playerActions <- use (player . actions)
      if playerActions <= 0
        then return ()
        else actionPhase evalUi player pNo
    else
      return ()

runTurn :: (MonadState GameState m, MonadWriter [String] m, MonadIO m)
        => GameData -> m ()
runTurn (GameData ip1 ip2) = do
  player1 %= initPhase
  player2 %= initPhase
  tickPhase
  player1 %= drawPhase
  player2 %= drawPhase
  damagePhase (imToEvalUi ip1) player1 1
  damagePhase (imToEvalUi ip2) player2 2
  actionPhase (imToEvalUi ip1) player1 1
  actionPhase (imToEvalUi ip2) player2 2
  player1 %= wardPhase
  player2 %= wardPhase
  turn %= (+ 1)

runTurn'' :: GameData -> GameState -> IO GameState
runTurn'' gd gs = do
  (newGs, log) <- runWriterT $ execStateT (runTurn gd) gs
  mapM_ putStrLn log
  return newGs

runTurnX :: Int -> GameState -> IO GameState
runTurnX 0 gs = do
  putStrLn $ drawGameState gs
  return gs
runTurnX i gs = do
  putStrLn $ "----- " ++ show i ++ "\n"
  putStrLn $ drawGameState gs
  newGS <- runTurn'' (GameData Cli Strat) gs
  runTurnX (i-1) newGS
