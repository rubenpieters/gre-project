module LibSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Generic.Random.Generic

import Control.Lens hiding (elements)
import Lib hiding (main)

instance Arbitrary Location where
  arbitrary = genericArbitrary' Z uniform

instance Arbitrary Target where
  arbitrary = genericArbitrary' Z uniform

instance Arbitrary CardModType where
  arbitrary = genericArbitrary' Z uniform

instance Arbitrary CardCappingType where
  arbitrary = genericArbitrary' Z uniform

instance Arbitrary CardProps where
  arbitrary = genericArbitrary' Z uniform

instance Arbitrary Card where
  arbitrary = genericArbitrary' Z uniform

instance Arbitrary PlayerState where
  arbitrary = genericArbitrary' Z uniform

instance Arbitrary CheckTransform where
  arbitrary = elements [ CheckTransform AvgCardVal SetAll
                       ]

testPlayer_EmptyDeckHand = PlayerState {_hand = [], _deck = [], _board = [NumberCard 0, NumberCard 0, NumberCard 0], _winner = False}
testGS_All0 = GameState {_playerState = [testPlayer_EmptyDeckHand, testPlayer_EmptyDeckHand], _playerTurn = 0, _turnCount = 0}

testPlayer_Board123 = PlayerState {_hand = [], _deck = [], _board = [NumberCard 1, NumberCard 2, NumberCard 3], _winner = False}
testGS_Board123 = GameState {_playerState = [testPlayer_Board123, testPlayer_Board123], _playerTurn = 0, _turnCount = 0}

testPlayer_HandDeck0 = PlayerState {_hand = replicate 3 (NumberCard 0), _deck = replicate 3 (NumberCard 0), _board = replicate 3 (NumberCard 0), _winner = False }
testGS_HandDeck0 = GameState {_playerState = [testPlayer_HandDeck0, testPlayer_HandDeck0], _playerTurn = 0, _turnCount = 0}

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "locAsLens" $ do
    it "should work in board case" $ property $ do
      \ps -> _board ps == ps ^. locAsLens Board
    it "should work in deck case" $ property $ do
      \ps -> _deck ps == ps ^. locAsLens Deck
    it "should work in hand case" $ property $ do
      \ps -> _hand ps == ps ^. locAsLens Hand
  describe "discardCard" $ do
    it "should decrease hand size" $ property $ do
      \ps -> length (discardCard ps ^. hand) <= length (ps ^. hand)
  describe "card3" $ do
    it "should work in basic case" $ do
      let newBoard = playCard 0 card3 testGS_All0
      newBoard `shouldBe` (testGS_All0 & (playerState . element 0 . board) .~ replicate 3 (NumberCard 2))
  describe "card7" $ do
    it "should work in basic case" $ do
      let newBoard = playCard 0 card7 testGS_HandDeck0
      newBoard `shouldBe` (testGS_HandDeck0 & (playerState . element 0 . deck) .~ replicate 3 (NumberCard 2))
  describe "card8" $ do
    it "should work in basic case" $ do
      let newBoard = playCard 0 card8 testGS_Board123
      newBoard `shouldBe` testGS_All0
