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
  , _block :: Bool
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


tick :: Timer -> Either CardEffect Timer
tick t = if t ^. countDown == 1
  then Left $ t ^. countDownEffect
  else Right $ t & countDown %~ (\x -> x - 1)

-- TODO: think of rules with multiple blocking timers
firstBlocking :: [Timer] -> Maybe (Int, Timer)
firstBlocking l = zip [0..] l ^? traverse . filtered (^. _2 . block)