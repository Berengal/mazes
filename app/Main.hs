module Main where

import Maze.Generate.Worm
import Maze.Picture
import System.Random
import Codec.Picture.Png
import qualified Data.Sequence as S

size = (500, 500)
scale = 1

main :: IO ()
main = do
  g <- newStdGen
  let grid = runWithRandomWorm g config
--  let grid = runDefaultBurrow g size
  writePng "maze.png" (gridToScaledImage grid scale)

config = BurrowConfig
  { gridSize = size
  , worms = S.Empty
  , turnChance = 0.02
  , breachWallChance = 0.00
  , spawnChance = 0.005
  , avoidSquares = True
  , respawnWorms = True
  , fillLeftoverDirt = False
  , randomWormSelection = False
  , avoidWalls = False
  }

organic = BurrowConfig
  { gridSize = size
  , worms = S.Empty
  , turnChance = 0.90
  , breachWallChance = 0.00
  , spawnChance = 0.10
  , avoidSquares = True
  , respawnWorms = False
  , fillLeftoverDirt = False
  , randomWormSelection = False
  , avoidWalls = True
  }

gnarly = BurrowConfig
  { gridSize = size
  , worms = S.empty
  , turnChance = 0.90
  , breachWallChance = 0.00
  , spawnChance = 0.20
  , avoidSquares = True
  , respawnWorms = False
  , fillLeftoverDirt = False
  , randomWormSelection = False
  , avoidWalls = True
  }

noisy = BurrowConfig
  { gridSize = size
  , worms = S.empty
  , turnChance = 0.90
  , breachWallChance = 0.00
  , spawnChance = 0.008
  , avoidSquares = True
  , respawnWorms = True
  , fillLeftoverDirt = False
  , randomWormSelection = True
  , avoidWalls = True
  }

highlyRegular = BurrowConfig
  { gridSize = size
  , worms = S.empty
  , turnChance = 0.10
  , breachWallChance = 0.00
  , spawnChance = 0.008
  , avoidSquares = True
  , respawnWorms = True
  , fillLeftoverDirt = False
  , randomWormSelection = False
  , avoidWalls = True
  }
