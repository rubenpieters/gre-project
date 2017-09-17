{-# LANGUAGE TemplateHaskell #-}

module Timer
  where

import Control.Lens
import Control.Monad
import Control.Monad.Operational

import Card

data Timer = Timer
  { _countDown :: Int
  , _block :: Bool
  , _timerOrigin :: Origin
  , _countDownEffect :: CardEffect
  }

makeLenses ''Timer

drawTimer :: Timer -> String
drawTimer t = show cd
  where
    cd = _countDown t

{-timerDmg :: Int -> Int -> DeckColumn -> Timer
timerDmg dmg cd col = Timer
  { _countDown = cd
  , _countDownEffect = do
      logG ("dmg " ++ show dmg)
      replicateM_ dmg (addCard dmgCard col)
  }
-}


tick :: Timer -> Either (CardEffect, Origin) Timer
tick t = if t ^. countDown == 1
  then Left $ (t ^. countDownEffect, t ^. timerOrigin)
  else Right $ t & countDown %~ (\x -> x - 1)

-- TODO: think of rules with multiple blocking timers
firstBlocking :: [Timer] -> Maybe (Int, Timer)
firstBlocking l = zip [0..] l ^? traverse . filtered (^. _2 . block)
