module Main where

import Maze.Generate.Worm
import Maze.Picture
import System.Random
import Codec.Picture.Png
import qualified Data.Set as S

config = BurrowConfig
  { gridSize = size
  , worms = S.empty
  , turnChance = 0.80
  , breachWallChance = 0.00
  , spawnChance = 0.20
  , avoidSquares = True
  , respawnWorms = True
  , fillLeftoverDirt = False
  , randomWormSelection = False
  }

size = (100, 100)

main :: IO ()
main = do
  g <- newStdGen
  let grid = runWithRandomWorm g config
--  let grid = runDefaultBurrow g size
  writePng "maze.png" (gridToScaledImage grid 4)
