module Maze.Generate.Worm.Spec (spec) where

import Maze.Generate.Worm

import Test.Hspec
import Test.QuickCheck

spec = do
  describe "directions" $ do
    it "should be consistent between `direction` and `moveDir`" $ property $
      \n -> direction (Pos (5, 7)) (moveDir (Pos (5, 7)) n) == n


instance Arbitrary Direction where
  arbitrary = elements [minBound, maxBound]

