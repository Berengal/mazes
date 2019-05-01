module Maze.Generate.Worm.Spec (spec) where

import Maze.Generate.Worm
import Maze.Generate.Worm.Internal

import Test.Hspec
import Test.QuickCheck

import Data.Ix
import Data.List

spec = do
  describe "directions" $ do
    it "should be consistent between `direction` and `moveDir`" $ property $
      \(n :: Direction) -> direction (Pos (5, 7)) (moveInDir (Pos (5, 7)) n) == n
  describe "spiral" $ do
    it "should generate all positions in bounds" $ property $
      allPositionsProp


allPositionsProp start bnds = sort (spiral start bnds) === (map Pos $ range bnds)

instance Arbitrary Direction where
  arbitrary = elements [minBound, maxBound]

instance Arbitrary Position where
  arbitrary = Pos <$> arbitrary
