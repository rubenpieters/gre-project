{-# LANGUAGE OverloadedStrings #-}

module Home
  ( homeView
  ) where

import Control.Monad (forM_)

import Lib

import Text.Blaze.XHtml5 ((!))
import qualified Text.Blaze.XHtml5 as H
import qualified Text.Blaze.XHtml5.Attributes as A

homeView :: H.Html
homeView = do
     H.head $ do
         H.title "GRE-1"
     H.body $ do
         H.p "Cards:"
         H.ul $ do
           forM_ cards cardSelect

type CardAmt = (Card, Int)

cardSelect :: CardAmt -> H.Html
cardSelect (c, i) = H.li $ do
  H.toHtml (show c ++ ", count: " ++ show i)
  H.button "+1" ! A.onclick "bar"
  H.button "-1"

cards :: [CardAmt]
cards = [(NumberCard 1, 0)
        , (BuffCard Board Friendly (Add 5), 0)
        ]
