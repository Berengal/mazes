module Main where

import Maze.Generate.Worm
import Maze.Picture
import System.Random
import Codec.Picture.Png
import qualified Data.Set as S

config = BurrowConfig
  { gridSize = (600, 600)
  , worms = S.empty
  , turnChance = 0.80
  , breachWallChance = 0.02
  , spawnChance = 0.15
  }

main :: IO ()
main = do
  g <- newStdGen
--  let grid = runWithRandomWorm g config
  let grid = runDefaultBurrow g (600, 600)
  writePng "maze.png" (gridToScaledImage grid 2)
