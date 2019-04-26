{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Maze.Generate.Worm


where

import System.Random

import Control.Monad
import Control.Monad.ST

import qualified Data.Set as S
import Data.Set ((\\))
import Data.List (sort, group)

import Data.Maybe

import Data.Array.ST
import Data.Array.IArray

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as TB

import qualified Data.Text.Lazy.IO as LTIO

data BurrowState s =
  BS { grid :: Grid s
     , worms :: S.Set Worm
     , breachWall :: forall g. (RandomGen g) => g -> Bool
     , traverseEmpty :: forall g. (RandomGen g) => g -> Bool
     , turn :: forall g. (RandomGen g) =>
               g
            -> Direction
            -> [Direction]
            -> Direction
     , spawnWorm :: forall g. (RandomGen g) =>
                    g
                 -> Position
                 -> [Direction]
                 -> Maybe Worm
     }

type Grid s = STArray s GridCoord Tile
type IGrid = Array GridCoord Tile

data Worm = Worm Position Direction
  deriving (Eq, Ord, Show)

newtype Position = Pos (Int, Int)
  deriving (Eq, Ord, Show)

unPos (Pos c) = c

data Direction
  = North
  | East
  | South
  | West
  deriving (Show, Eq, Ord, Enum, Bounded)
          
data Tile
  = Dirt
  | Empty
  | Wall
  | Solid TileAnnotation
  deriving (Show, Eq, Ord)

data TileAnnotation
  = Edge
  | Internal
  | Entry
  | Exit
  deriving (Show, Eq, Ord)

type GridCoord = (Int, Int)
type GridBounds = (GridCoord, GridCoord)

defaultBurrowState :: (Int, Int) -> ST s (BurrowState s)
defaultBurrowState size = do
  grid <- newGrid size
  return BS {..}
  where worms = S.empty
  
        breachWall g = fst (random g) < breachChance
        
        traverseEmpty g = fst (random g) < traverseChance
        
        turn g currentDir preferedDirs =
          let (r, g1) = random g
              --filterDirs = filter (flip elem [currentDir, opposite currentDir])
              --filtered = filterDirs preferedDirs
              dirs = if null preferedDirs
                     then directions
                     else preferedDirs
          in if r < turnChance then
            chooseL g1 dirs
          else
            currentDir
            
        spawnWorm g pos dirs =
          let (r, g1) = random g
          in if r < spawnChance && not (null dirs) then
            Just (Worm pos (chooseL g1 dirs))
          else
            Nothing

breachChance, traverseChance, turnChance, spawnChance :: Double
breachChance = 0.03
traverseChance = 0.95
turnChance = 0.70
spawnChance = 0.20

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

randomWorm :: (RandomGen g) => g -> GridBounds -> Worm
randomWorm g ((lowX, lowY), (highX, highY))
  = Worm (Pos (fst (randomR (lowX, highX) g1)
              ,fst (randomR (lowY, highY) g2)))
         (chooseL g3 directions)
    
  where (g1:g2:g3:_) = splits g

addWorm :: BurrowState s -> Worm -> BurrowState s
addWorm BS{..} worm = BS{worms = S.union worms (S.singleton worm), ..}

replaceWorms :: BurrowState s -> S.Set Worm -> BurrowState s
replaceWorms bs worms = bs {worms = worms}

step :: forall s g. (RandomGen g) => g -> BurrowState s -> ST s (Maybe (S.Set Worm))
step g BS{..} =
  if S.null worms then
    return Nothing
  else do
    tile <- getTileInFront oldWorm grid
    case tile of
      Solid _ -> return deadWormSet
      Wall    -> if not (breachWall dieG) then
                   return deadWormSet
                 else
                   moveWorm
      Empty   -> if not (traverseEmpty dieG) then
                   return deadWormSet
                 else
                   moveWorm
      Dirt    -> moveWorm
    
  where wormG:dieG:turnG:spawnG:_ = splits g
  
        wormIx = fst $ randomR (0, S.size worms - 1) wormG
        oldWorm@(Worm oldPos oldDir) = S.elemAt wormIx worms
        noWormSet = worms \\ S.singleton oldWorm
        deadWormSet = Just noWormSet
        
        newPos@(Pos (x, y)) = moveDir oldPos oldDir
       

        moveWorm :: ST s (Maybe (S.Set Worm))
        moveWorm = do
          neighborTiles <- mapM (readArray grid . unPos) (neighbors newPos)
          let dirtDirections = map (direction newPos . fst)
                               . filter ((==Dirt).snd)
                               . zip (neighbors newPos)
                               $ neighborTiles
          updateGrid dirtDirections

        updateGrid :: [Direction] -> ST s (Maybe (S.Set Worm))
        updateGrid dirtDirections = do
          writeArray grid (x, y) Empty
          forM_ (neighbors newPos) \neighbor ->
            unless (isTileInFrontOf newWorm neighbor
                    || maybe False (flip isTileInFrontOf neighbor) spawnedWorm) $ do
              tile <- readArray grid (unPos neighbor)
              case tile of
                Dirt -> fillAvoidingSquare grid neighbor
                _    -> return ()
              
          return spawnedWormSet
          
          where
            preferedDirs = if null dirtDirections then directions else dirtDirections
            newDir = turn turnG oldDir preferedDirs
            newWorm = Worm newPos newDir
            spawnedWorm = spawnWorm wormG newPos (filter (/= newDir) preferedDirs)
        
            movedWormSet = S.insert newWorm noWormSet
            spawnedWormSet = Just case spawnedWorm of
                               Nothing -> movedWormSet
                               Just w -> S.insert w movedWormSet

fillAvoidingSquare :: STArray s (Int, Int) Tile -> Position -> ST s ()
fillAvoidingSquare grid pos = do
  avoidedSquares <- forM (squaresWith pos) $ \square -> do
    -- Look for 3 tiles in a square that are either Empty or Wall/Solid
    squareTiles <- map (\(_,t) -> if t then Empty else Wall)
                   . filter ((==3).fst)
                   . map (\ts -> (length ts, head ts))
                   . group
                   . sort
                   . map (==Empty)
                   . filter (/=Dirt)
                   <$> mapM (readArray grid . unPos) square
    forM_ squareTiles \(tile) ->
      case tile of
        Empty -> writeArray grid (unPos pos) (Solid Internal)
        Wall  -> writeArray grid (unPos pos) Dirt --Should already be dirt
        _     -> return ()
    return (not (null squareTiles))
  if or avoidedSquares then return ()
    else writeArray grid (unPos pos) Wall

runBurrow :: (RandomGen g) => g -> (Int, Int) -> IGrid
runBurrow g (x, y) = runST $ do
  bs <- defaultBurrowState (x, y)
  let bs' = addWorm bs (randomWorm g1 ((1,1),(x , y)))
  stepUntilAllDead g2 bs'
  freeze (grid bs')
  
  where (g1:g2:_) = splits g
        stepUntilAllDead :: (RandomGen g) => g -> BurrowState s -> ST s ()
        stepUntilAllDead g bs = do
          let (g', g'') = split g
          newWorms <- step g' bs
          case newWorms of
            Nothing -> return ()
            Just nw -> stepUntilAllDead g'' (replaceWorms bs nw)

isTileInFrontOf :: Worm -> Position -> Bool
isTileInFrontOf (Worm wormPos wormDir) pos
  = pos == moveDir wormPos wormDir
        
getTileInFront :: MArray a b m => Worm -> a (Int, Int) b -> m b
getTileInFront (Worm pos dir) grid = do
  bounds <- getBounds grid
  let (Pos (x, y)) = moveDir pos dir
  readArray grid (x, y)

opposite :: Direction -> Direction
opposite North = South
opposite South = North
opposite East = West
opposite West = East

directions :: [Direction]
directions = [North, South, East, West]

neighbors :: Position -> [Position]
neighbors (Pos (x, y)) =
  [ Pos (x-1, y)
  , Pos (x+1, y)
  , Pos (x, y-1)
  , Pos (x, y+1)
  ]

direction :: Position -> Position -> Direction
direction (Pos (x1, y1)) (Pos (x2, y2))
  | x1 > x2 = West
  | x2 > x1 = East
  | y1 > y2 = South
  | y2 > y1 = North

squareAt :: Position -> [Position]
squareAt (Pos (x,y)) = [Pos (x+dx, y+dy) | dx <- [0,1], dy <- [0,1]]

squaresWith :: Position -> [[Position]]
squaresWith (Pos (x,y)) = map squareAt . squareAt $ Pos (x-1, y-1)

moveDir :: Position -> Direction -> Position
moveDir (Pos (x, y)) dir = case dir of
  North -> Pos (x, y+1)
  South -> Pos (x, y-1)
  East  -> Pos (x+1, y)
  West  -> Pos (x-1, y)

showGrid :: IGrid -> LT.Text
showGrid arr = TB.toLazyTextWith (rangeSize ((0,0),(x,y))) builder
  where tileToChar Dirt = '-'
        tileToChar Empty = ' '
        tileToChar Wall = '#'
        tileToChar (Solid Entry) = '?'
        tileToChar (Solid Exit) = '+'
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

splits g = let (g', gs) = split g
           in g':splits gs
