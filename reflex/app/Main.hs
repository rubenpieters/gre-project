{-# LANGUAGE RecursiveDo #-}

module Main where

import Control.Monad

import Reflex
import Reflex.Dom

import Data.Map as M

main :: IO ()
main = mainWidget $ el "div" $ do
  rec
    el "h1" $ text "test"
    let initialList = M.fromList [(i,0) | i <- [0..2 :: Int]]
    clicks <- listWithKey (constDyn initialList) $ \k v -> do
      rec
        text (show k)
        text " - "
        display nClicks
        let btnClick = button "+1"
        nClicks <- count =<< btnClick
      return (nClicks :: Dynamic Spider Int)
    let clicksJoined = joinDynThroughMap clicks
    let clicks' = mapDyn (M.elems) clicksJoined
    el "div" $ display =<< clicks'
    runClick <- button "run"
  return ()

listx = ["1", "2", "3"]

countingBtn :: MonadWidget t m => m (Dynamic t Int)
countingBtn = count =<< button "+1"
