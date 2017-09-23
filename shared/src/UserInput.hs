{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module UserInput
  where

import Data.List

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Operational
import Control.Lens hiding (view)

import Card
import Hand
import Strategy

data UserInputOp a where
  PlayHand :: [Card] -> [Card] -> UserInputOp Int

playHand a b = singleton $ PlayHand a b

data InputMode = Cli | Strat | Man
  deriving (Eq, Show, Ord)

data GameData = GameData
  { inputP1 :: InputMode
  , inputP2 :: InputMode
  }

makeLenses ''GameData

type UIHandler m = forall a. Program UserInputOp a -> m a

imToEvalUi :: (MonadIO m) => InputMode -> UIHandler m
imToEvalUi Cli = evalUiCli
imToEvalUi Strat = evalUiStrat
imToEvalUi Man = undefined

evalUiCli :: (MonadIO m) => Program UserInputOp a -> m a
evalUiCli = eval . view
  where
    eval :: (MonadIO m) => ProgramView UserInputOp a -> m a
    eval (Return x) = return x
    eval (PlayHand _ choiceH :>>= k) = do
      liftIO $ putStrLn "choose card to play:"
      liftIO $ putStrLn $ drawCards choiceH
      let choices = [0..(length choiceH-1)]
      liftIO $ putStrLn $ " " ++ intercalate "  " (show <$> reverse choices)
      i <- readLnGuarded (`elem` choices)
      evalUiCli (k i)

readLnGuarded :: (MonadIO m, Read a) => (a -> Bool) -> m a
readLnGuarded f = do
  i <- liftIO readLn
  if f i
    then return i
    else do
      liftIO $ putStrLn "invalid choice"
      readLnGuarded f

evalUiStrat :: (MonadIO m) => Program UserInputOp a -> m a
evalUiStrat = eval . view
  where
    eval :: (MonadIO m) => ProgramView UserInputOp a -> m a
    eval (Return x) = return x
    eval (PlayHand _ choiceH :>>= k) = do
      let cardIx = handStrategy choiceH
      evalUiStrat (k cardIx)
