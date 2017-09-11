{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module UserInput
  where

import Data.List

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Operational
import Control.Lens hiding (view)

import Card
import Hand

data UserInputOp a where
  PlayHand :: [Card] -> UserInputOp Int

playHand a = singleton $ PlayHand a

evalUiCli :: (MonadIO m) => Program UserInputOp a -> m a
evalUiCli = eval . view
  where
    eval :: (MonadIO m) => ProgramView UserInputOp a -> m a
    eval (Return x) = return x
    eval (PlayHand h :>>= k) = do
      liftIO $ putStrLn "choose card to play:"
      liftIO $ putStrLn $ drawCards h
      liftIO $ putStrLn $ " " ++ intercalate "  " (show <$> [0..(length h-1)])
      i <- liftIO readLn
      evalUiCli (k i)
