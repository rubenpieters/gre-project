{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module UserInput
  where

import Data.List

import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class
import Control.Monad.Operational
import Control.Lens hiding (view)

import Player
import Card
import Hand
import Strategy

data UserInputOp a where
  PlayHand :: [(Card, Int)] -> UserInputOp Int
  ClearDmg :: UserInputOp Bool

playHand a = singleton $ PlayHand a
clearDmg = singleton ClearDmg

data InputMode = Cli | Strat | Man (Int -> Int)

data GameData = GameData
  { inputP1 :: InputMode
  , inputP2 :: InputMode
  }

makeLenses ''GameData

type UIHandler m = forall a. Program UserInputOp a -> m a

imToEvalUi :: (MonadState GameState m, MonadIO m)
           => InputMode -> UIHandler m
imToEvalUi Cli = evalUiCli
imToEvalUi Strat = evalUiStrat
imToEvalUi (Man f) = evalUiMan f

evalUiCli :: (MonadIO m)
          => Program UserInputOp a -> m a
evalUiCli = eval . view
  where
    eval :: (MonadIO m) => ProgramView UserInputOp a -> m a
    eval (Return x) = return x
    eval (PlayHand choiceH :>>= k) = do
      liftIO $ putStrLn "choose card to play:"
      liftIO $ putStrLn $ drawCards (fst <$> choiceH)
      let choices = [0..(length choiceH-1)]
      liftIO $ putStrLn $ " " ++ intercalate "   " (show <$> reverse choices)
      i <- readLnGuarded (`elem` choices)
      evalUiCli (k ((snd <$> choiceH) !! i))
    eval (ClearDmg :>>= k) = do
      liftIO $ putStrLn "clear damage?"
      b <- liftIO readLn
      evalUiCli (k b)

readLnGuarded :: (MonadIO m, Read a) => (a -> Bool) -> m a
readLnGuarded f = do
  i <- liftIO readLn
  if f i
    then return i
    else do
      liftIO $ putStrLn "invalid choice"
      readLnGuarded f

evalUiStrat :: (MonadIO m)
            => Program UserInputOp a -> m a
evalUiStrat = eval . view
  where
    eval :: (MonadIO m) => ProgramView UserInputOp a -> m a
    eval (Return x) = return x
    eval (PlayHand choiceH :>>= k) = do
      let i = handStrategy (fst <$> choiceH)
      evalUiStrat (k ((snd <$> choiceH) !! i))
    eval (ClearDmg :>>= k) =
      evalUiStrat (k True)

evalUiMan :: (MonadState GameState m, MonadIO m)
          => (Int -> Int) -> Program UserInputOp a -> m a
evalUiMan f = eval . view
  where
    eval :: (MonadState GameState m, MonadIO m)
         => ProgramView UserInputOp a -> m a
    eval (Return x) = return x
    eval (PlayHand _ :>>= k) = do
      t <- use turn
      evalUiMan f (k (f t))
