module LibSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Generic.Random.Generic

import Control.Lens
import Lib

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
