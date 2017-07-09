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

instance Arbitrary RangeType where
  arbitrary = genericArbitrary' Z uniform

instance Arbitrary CardProps where
  arbitrary = genericArbitrary' Z uniform

instance Arbitrary CardFilter where
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

testPlayer_WithBoard b = PlayerState {_hand = [], _deck = [], _board = b, _winner = False}
testGS_B1B2 b1 b2 = GameState {_playerState = [testPlayer_WithBoard b1, testPlayer_WithBoard b2], _playerTurn = 0, _turnCount = 0}

testPlayer_WithDeck d = PlayerState {_hand = [], _deck = d, _board = [NumberCard 1], _winner = False}
testGS_D1D2 d1 d2 = GameState {_playerState = [testPlayer_WithDeck d1, testPlayer_WithDeck d2], _playerTurn = 0, _turnCount = 0}

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
  describe "card1" $ do
    it "should work in basic case" $ do
      newBoard <- playCard 0 card1 testGS_Board123
      newBoard `shouldBe` (testGS_Board123 & (playerState . element 0 . board . element 0) .~ NumberCard 10)
  describe "card2" $ do
    it "should work in basic case" $ do
      newBoard <- playCard 0 card2 testGS_Board123
      newBoard `shouldBe` (testGS_Board123 & (playerState . element 0 . board . element 0) .~ NumberCard 6
                                           & (playerState . element 1 . board . element 0) .~ NumberCard 6
                          )
  describe "card3" $ do
    it "should work in basic case" $ do
      newBoard <- playCard 0 card3 testGS_All0
      newBoard `shouldBe` (testGS_All0 & (playerState . element 0 . board) .~ replicate 3 (NumberCard 2))
  describe "card4" $ do
    it "should work in basic case" $ do
      let gs = testGS_B1B2 (map NumberCard [0, 1, 0]) (map NumberCard [0, 1, 0])
      newBoard <- playCard 0 card4 gs
      newBoard `shouldBe` (gs & (playerState . element 0 . board) .~ replicate 3 (NumberCard 1))
    it "should work in highest is rightmost case" $ do
      let gs = testGS_B1B2 (map NumberCard [0, 0, 1]) (map NumberCard [0, 1, 0])
      newBoard <- playCard 0 card4 gs
      newBoard `shouldBe` (gs & (playerState . element 0 . board) .~ map NumberCard [0, 1, 1])
    it "should work in highest is leftmost case" $ do
      let gs = testGS_B1B2 (map NumberCard [1, 0, 0]) (map NumberCard [0, 1, 0])
      newBoard <- playCard 0 card4 gs
      newBoard `shouldBe` (gs & (playerState . element 0 . board) .~ map NumberCard [1, 1, 0])
  describe "card5" $ do
    it "should work in basic case" $ do
      newBoard <- playCard 0 card5 testGS_Board123
      newBoard `shouldBe` (testGS_Board123 & (playerState . element 0 . board) .~ map NumberCard [0, 2, 4])
  describe "card6" $ do
    it "should work in basic case " $ do
      newBoard <- playCard 0 card6 testGS_Board123
      newBoard `shouldBe` (testGS_Board123 & (playerState . element 0 . board) .~ map NumberCard [2, 2, 2]
                                           & (playerState . element 1 . board) .~ map NumberCard [2, 2, 2]
                          )
  describe "card7" $ do
    it "should work in basic case" $ do
      newBoard <- playCard 0 card7 testGS_HandDeck0
      newBoard `shouldBe` (testGS_HandDeck0 & (playerState . element 0 . deck) .~ replicate 3 (NumberCard 2))
  describe "card8" $ do
    it "should work in basic case" $ do
      newBoard <- playCard 0 card8 testGS_Board123
      newBoard `shouldBe` testGS_All0
  describe "card9" $ do
    it "should work in basic case" $ do
      let gs = testGS_D1D2 (map NumberCard [1, 2]) (map NumberCard [1,2])
      newBoard <- playCard 0 card9 gs
      let newDeck = map NumberCard [0, 1]
      newBoard ^.. (playerState . element 1 . deck . traverse) `shouldBe` newDeck
      newBoard `shouldBe` (gs & (playerState . element 1 . deck) .~ newDeck)
  describe "card10" $ do
    it "should work in basic case" $ do
      let gs = testGS_D1D2 (map NumberCard [1,2,3,4]) (map NumberCard [1,2,3,4])
      newBoard <- playCard 0 (card10 (NumberCard 3) (NumberCard 4)) gs
      let newDeck = map NumberCard [3,4,1,2]
      newBoard ^.. (playerState . element 0 . deck . traverse) `shouldBe` newDeck
      newBoard `shouldBe` (gs & (playerState . element 0 . deck) .~ newDeck)
  describe "card12" $ do
    it "should work in basic case" $ do
      newBoard <- playCard 0 card12 testGS_Board123
      newBoard `shouldBe` (testGS_Board123 & (playerState . element 0 . board . element 2) .~ NumberCard 8)

