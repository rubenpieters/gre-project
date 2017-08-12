{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Lens
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Trans
import Control.Monad.Random

import Reflex
import Reflex.Dom

import Text.Read

import Data.Maybe
import qualified Data.Map as M

import GameResult
import Lib hiding (main)

main :: IO ()
main = mainWidget $ el "div" $ do
  rec
    el "h1" $ text "GRE - prototype 1 v0.1"
    text "Deck 1"
    deck1 <- deckSelection
    text "Deck2"
    deck2 <- deckSelection
    let selectDeck d = fmap (const $ M.elems d) runClick
    selectedDeck1Event <- sampleMap selectDeck deck1
    selectedDeck2Event <- sampleMap selectDeck deck2
    let combinedDecks = combineEvents selectedDeck1Event selectedDeck2Event
    gameResult <- performArg (\(d1, d2) -> runGameIO $ runGame d1 d2 cards) combinedDecks
    let gameResultTuple = holdDyn (-1, -1, 0) ((matchResultOutput . fst) <$> gameResult)
    --let gameLog = holdDyn [] (fmap snd $ test'')
    text "Win | Loss | Played "
    display =<< gameResultTuple
    --display =<< gameLog
    runClick <- button "run"
  return ()

combineEvents :: (Reflex t) => Event t a -> Event t b -> Event t (a, b)
combineEvents e1 e2 = coincidence $ nestedEvents eventInside
  where
    eventInside = fmap (e1,) e2
    nestedEvents e = fmap (\(a,b) -> fmap (,b) a) e

sampleMap :: (MonadHold t m, Reflex t) => (a -> Event t b) -> Dynamic t a -> m (Event t b)
sampleMap f d = do
  x <- mapDyn f d
  return $ switchPromptlyDyn x

deckSelection :: (MonadWidget t m) => m (Dynamic t (M.Map Card Int))
deckSelection = do
  deck <- joinDynThroughMap <$> nestedDeckMap
  --let deckValues = fmap (M.elems) deck
  --el "div" $ display deckValues
  return deck
  where
    nestedDeckMap :: (MonadWidget t m) => m (Dynamic t (M.Map Card (Dynamic t Int)))
    nestedDeckMap = listWithKey (constDyn cardList) $ \k _ -> el "li" $ cardAddButton k

cardAddButton :: (MonadWidget t m, Show a) => a -> m (Dynamic t Int)
cardAddButton k = do
  rec
    cardAmtInput <- textInput $ def & textInputConfig_initialValue .~ "0"
    text " - "
    display cardAmtInt
    text (" - " ++ show k)
    let cardAmt = _textInput_value cardAmtInput
    cardAmtInt <- mapDyn readOr0 cardAmt
  return cardAmtInt

readOr0 :: (Read a, Num a) => String -> a
readOr0 a = fromMaybe 0 (readMaybe a)

matchResultOutput :: MatchResult -> (Int, Int, Int)
matchResultOutput (MatchResult a b c) = (a, b, c)

runGame :: (MonadWriter [GameLog] m, MonadRandom m) => [Int] -> [Int] -> [Card] -> m MatchResult
runGame d1 d2 cards = playMatch (assemble (zip cards d1)) (assemble (zip cards d2)) 5

performArg :: MonadWidget t m => (b -> IO a) -> Event t b -> m (Event t a)
performArg f x = performEvent (fmap (liftIO . f) x)

otherDeck :: [Card]
otherDeck = replicate 1000 card1

assemble :: [(Card, Int)] -> [Card]
assemble l = foldr (\(c, i) -> (++) (replicate i c)) [] l

cardList :: M.Map Card Int
cardList = M.fromList (zip cards [0..])

cards :: [Card]
cards = [card1
        , card2
        , card3
        , card4
        , card5
        , card6
        , card7
        , card8
        , card9
        --, card10
        , card11
        , card12
        , card13
        , card14
        --, card15
        --, card16
        , card17
        , card18
        --, card19
        ]

listx = ["1", "2", "3"]

countingBtn :: MonadWidget t m => m (Dynamic t Int)
countingBtn = count =<< button "+1"
