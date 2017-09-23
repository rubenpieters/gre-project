module GameStateSpec (main, spec) where

import Control.Lens

import Test.Hspec

import Deck
import Card
import Player
import GameState
import UserInput

emptyDeck = Deck
  { _frontL = []
  , _frontM = []
  , _frontR = []
  , _backL = []
  , _backM = []
  , _backR = []
  }

emptyPlayer = Player
  { _deck = emptyDeck
  , _hand = []
  , _timersL = []
  , _timersM = []
  , _timersR = []
  , _dps = 0
  , _actions = 0
  }

emptyGS = GameState
  { _player1 = emptyPlayer
  , _player2 = emptyPlayer
  , _turn = 0
  }

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "after playing dmg card" $ do
    it "should return back to the deck/add a timer" $ do
      let gs = emptyGS & (player1 . deck . frontL) .~ [dmg11Card]
      gs' <- runTurn'' (GameData Strat Strat) gs
      let test = gs' ^. (player1 . deck . frontL)
      let test2 = gs' ^. (player1 . timersL)
      length test `shouldBe` 1
      length test2 `shouldBe` 1

