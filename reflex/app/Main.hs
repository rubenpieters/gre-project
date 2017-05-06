{-# LANGUAGE RecursiveDo #-}

module Main where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Random

import Reflex
import Reflex.Dom

import qualified Data.Map as M

import Lib

main :: IO ()
main = mainWidget $ el "div" $ do
  rec
    el "h1" $ text "test"
    let initialList = M.fromList (zip cards (replicate (length cards) 0)) --[(i,0) | i <- [0..2 :: Int]]
    clicks <- listWithKey (constDyn initialList) $ \k v ->
      el "li" $ do
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
    test <- (mapDyn (\x -> fmap (const $ M.elems x) runClick) clicksJoined)
    let test' = switchPromptlyDyn test
    test'' <- performArg (flip runGame cards) test'
    let test''Dyn = holdDyn (0, 0) test''
    --gameResult <- fmap (mapDyn (\x -> testGame' (assemble (zip cards x)) otherDeck)) test'Dyn
    runClick <- button "run"
    display =<< test''Dyn
  return ()

runGame :: (MonadRandom m) => [Int] -> [Card] -> m (Int, Int)
runGame nrs cards = playXTimes (assemble (zip cards nrs)) otherDeck 20

performArg :: MonadWidget t m => (b -> IO a) -> Event t b -> m (Event t a)
performArg f x = performEvent (fmap (liftIO . f) x)

otherDeck :: [Card]
otherDeck = [NumberCard 1, BuffCard Board Friendly (CardProps (Add 5) (MaxCap 100))]

assemble :: [(Card, Int)] -> [Card]
assemble l = foldr (\(c, i) -> (++) (replicate i c)) [] l

cards :: [Card]
cards = [NumberCard 1
        , BuffCard Board Friendly (CardProps (Add 5) (MaxCap 100))
        ]

listx = ["1", "2", "3"]

countingBtn :: MonadWidget t m => m (Dynamic t Int)
countingBtn = count =<< button "+1"
