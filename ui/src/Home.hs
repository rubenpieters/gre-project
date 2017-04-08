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
         H.ul $ forM_ cards (H.li . H.toHtml . show)

cards :: [Card]
cards = [NumberCard 1
        , BuffCard Board Friendly (Add 5)
        ]
