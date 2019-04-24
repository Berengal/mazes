{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Maze.Generate.Worm where

import System.Random

import Control.Monad
import Control.Monad.ST

import qualified Data.IntMap as M

import Data.Array.ST
import Data.Array.IArray

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as TB

import qualified Data.Text.Lazy.IO as LTIO

data BurrowState s =
  BS { grid :: Grid s
     , worms :: M.IntMap Worm
     , breachWall :: forall g. (RandomGen g) => g -> Bool
     , traverseEmpty :: forall g. (RandomGen g) => g -> Bool
     , turn :: forall g. (RandomGen g) => g -> Direction -> Direction
     , spawnWorm :: forall g. (RandomGen g) => g -> Position -> Direction -> Maybe Worm
     }

type Grid s = STArray s GridCoord Tile
type IGrid = Array GridCoord Tile

data Worm = Worm Position Direction

newtype Position = Pos (Int, Int)

data Direction
  = North
  | East
  | South
  | West
  deriving (Show, Eq, Enum)
          
data Tile
  = Dirt
  | Empty
  | Wall
  | Solid TileAnn
  deriving (Show, Eq)

data TileAnn
  = Edge
  | Internal
  | Exit
  deriving (Show, Eq)

type GridCoord = (Int, Int)
type GridBounds = (GridCoord, GridCoord)

defaultBurrowState :: (Int, Int) -> ST s (BurrowState s)
defaultBurrowState size = do
  grid <- newGrid size
  return BS {..}
  where worms = M.empty
  
        breachWall g = fst (random g) < breachChance
        
        traverseEmpty g = fst (random g) < traverseChance
        
        turn g currentDir =
          let (r, g1) = random g
          in if r < turnChance then
            chooseL g1 (filter (not . flip elem [currentDir, opposite currentDir]) directions)
          else
            currentDir
            
        spawnWorm g pos prevDir =
          let (r, g1) = random g
          in if r < spawnChance then
            Just (Worm pos (chooseL g1 (filter (/= prevDir) directions)))
          else
            Nothing

step :: (RandomGen g) => BurrowState s -> g -> ST s (BurrowState s)
step bs@BS{..} g =
  if M.null worms then
    return BS {..}
  else do
    tile <- getTileInFront worm grid
    case tile of
      Solid _ -> return deadWorm
      Wall    -> if not (breachWall dieG) then return deadWorm
                 else moveWorm
      Empty   -> if not (traverseEmpty dieG) then return deadWorm
                 else moveWorm
      Dirt    -> moveWorm
    
  where wormG:dieG:turnG:spawnG:_ = splits g
        wormIx = chooseL wormG (M.keys worms)
        worm@(Worm pos dir) = worms M.! wormIx
        deadWorm = BS { worms = M.delete wormIx worms, ..}
        movedWorm = Worm (moveDir pos dir) (turn turnG dir)
        moveWorm = do
          let (Pos (x, y)) = moveDir pos dir
          writeArray grid (x, y) Empty
          
          
getTileInFront (Worm pos dir) grid = do
  bounds <- getBounds grid
  let (Pos (x, y)) = moveDir pos dir
  readArray grid (x, y)

breachChance, traverseChance, turnChance, spawnChance :: Double
breachChance = 0.05
traverseChance = 0.15
turnChance = 0.20
spawnChance = 0.03

newGrid :: (Int, Int) -> ST s (Grid s)
newGrid (x, y) = do
  arr <- newArray ((0,0), (x+1,y+1)) Dirt
  forM_ [0..x+1] \i -> do
    writeArray arr (i, 0) (Solid Edge)
    writeArray arr (i, y+1) (Solid Edge)
  forM_ [0..y+1] \i -> do
    writeArray arr (0, i) (Solid Edge)
    writeArray arr (x+1, i) (Solid Edge)
  return arr

opposite :: Direction -> Direction
opposite North = South
opposite South = North
opposite East = West
opposite West = East

directions = [North, South, East, West]

moveDir :: Position -> Direction -> Position
moveDir (Pos (x, y)) dir = case dir of
  North -> Pos (x-1, y)
  South -> Pos (x+1, y)
  East  -> Pos (x, y+1)
  West  -> Pos (x, y-1)

showGrid :: IGrid -> LT.Text
showGrid arr = TB.toLazyTextWith (rangeSize ((0,0),(x,y))) builder
  where tileToChar Dirt = '*'
        tileToChar Empty = ' '
        tileToChar Wall = '#'
        tileToChar (Solid Exit) = '_'
        tileToChar (Solid _) = '!'
        charArr = amap tileToChar arr
        (_,(x,y)) = bounds arr
        builder = mconcat [mconcat chars <> TB.singleton '\n'
                          | x' <- [0..x]
                          , let chars = [TB.singleton . tileToChar $ arr ! (x', y')
                                        | y' <- [0..y]]]

printGrid :: IGrid -> IO ()
printGrid arr = LTIO.putStr (showGrid arr)

randomR1 :: (Random a, RandomGen g) => g -> (a, a) -> a
randomR1 g = fst . flip randomR g

chooseL :: RandomGen g => g -> [a] -> a
chooseL g list = list !! index
  where index = fst (randomR (0, len) g)
        len = (length list) - 1

chooseM :: RandomGen g => g -> M.IntMap a -> a
chooseM g im = im M.! i
  where i = chooseL g (M.keys im)
  
splits g = let (g', gs) = split g
           in g':splits gs
