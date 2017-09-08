{-# LANGUAGE TemplateHaskell #-}

module Timer
  where

import Control.Lens
import Control.Monad
import Control.Monad.Operational

import Card

data Timer = Timer
  { _countDown :: Int
  , _countDownEffect :: CardEffect
  }

makeLenses ''Timer

drawTimer :: Timer -> String
drawTimer = undefined

timerDmg :: Int -> Int -> DeckColumn -> Timer
timerDmg dmg cd col = Timer
  { _countDown = cd
  , _countDownEffect = replicateM_ dmg $ singleton (AddCard dmgCard col)
  }
