module LibSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Generic.Random.Generic

import Control.Lens

instance Arbitrary Location where
  arbitrary = genericArbitrary Z uniform

instance Arbitrary Target where
  arbitrary = genericArbitrary Z uniform

instance Arbitrary CardModType where
  arbitrary = genericArbitrary Z uniform

instance Arbitrary CardCappingType where
  arbitrary = genericArbitrary Z uniform

instance Arbitrary CardProps where
  arbitrary = genericArbitrary Z uniform

instance Arbitrary Card where
  arbitrary = genericArbitrary Z uniform

instance Arbitrary PlayerState where
  arbitrary = genericArbitrary Z uniform

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "discardCard" $ do
    it "should decrease hand size" $ property $ do
      \ps -> length (discardCard ps ^. hand) `shouldBe` length (ps ^. hand) - 1
