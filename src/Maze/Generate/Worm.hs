{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}

-- | Algorithm for generating mazes in a rectangular 2d grid.
  --
  --This algorithm works by having worms dig out a maze in a random fashion. As
  --the worms burrow through the region they have a random chance to turn, spawn
  --new worms, and if they're at a dead end, to tunnel through the wall in front
  --of them. The walls and empty tiles in the final maze are only 1 tile wide
  --with no larger open rooms or columns.
  --
  --All empty tiles created by any single worm or its spawn are connected in the
  --final maze. If there are multiple worms to start with the separate parts of
  --the maze depend on worms breaching walls to connect them together.
  --
  --The inputs to the algorithm are the initial grid configuration, initial
  --worms and weights determining the probability of a worm turning,
  --spawning a new worm and breaching a wall.
  --
  --The initial grid is assumed to be a rectangle of size (n+1,m+1) with a range of
  --(0,0) (n,m) (inclusive) where all the tiles at the edge are marked
  --`Solid`. The tiles inside the grid can be of any type, but usually they will
  --all be Dirt.
  --
  --`Solid` tiles are further annotated, and tiles annotated `Entry` or `Exit` will
  --guaranteed have a path to leading to them in the final maze.
  --
  --Worms can start at any position, including inside solid tiles which is
  --useful for starting at an entrance, but must face in a valid direction
  --(i.e. not off the grid).
  --
  --The turn chance determines how often the worm will turn. If it does turn it
  --has an equal chance to turn to any direction in which there's dirt, or any
  --direction at all if there's no dirt. This includes the previous direction it
  --was facing, so just because it chooses to turn it might still end up facing
  --the same direction
  --
  --When a new worm spawns it spawns in the same tile as it's parent but facing
  --a different direction, chosen randomly among all dirt tiles. If there are no
  --dirt tiles (other than maybe the one it's parent faces) it won't spawn.

  -- TODO actually guarantee tiles annotated Entry or Exit have paths leading to them
  -- TODO Spawn new worms when there's more dirt and no live worms
  -- TODO Sanity check on the initial state
  -- TODO Separate the running BurrowState from the input BurrowState (i.e. turn
  -- the STArray into an IArray or plain indexes (with optional fill function?)

module Maze.Generate.Worm
  ( -- * Data types
    BurrowState(..)
  , Grid
  , IGrid
  , Worm
  , Position(Pos)
  , Direction(..)
  , Tile(..)
  , TileAnnotation (..)
  , GridCoord
  , GridBounds

  -- * Algorithm
  , defaultBurrowState
  , newGrid

  , runDefaultBurrow
  , runBurrow

  -- * Utility functions
  , randomWorm
  , opposite
  , directions
  , neighbors
  , direction
  , squaresWith
  , moveInDir
  , showGrid
  , printGrid
  )

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

-- | The BurrowState contains the state used for running the algorithm

data BurrowState s =
  BS { grid :: Grid s -- ^ The maze grid
     , worms :: S.Set Worm -- ^ The set of active worms on the grid
     , breachWallChance :: Double -- ^ Chance that a worm will breach a wall
                           -- (between 0 and 1)
     , turnChance :: Double -- ^ Chance that a worm won't just continue straight
                     -- ahead at every tile
     , spawnChance :: Double -- ^ Chance to spawn a new worm at every tile
     }

-- | Array of tiles representing a 2d maze grid
type Grid s = STArray s GridCoord Tile
-- | Immutable maze grid
type IGrid = Array GridCoord Tile

-- | Worms burrow through the maze. Each has a position and a direction
data Worm = Worm Position Direction
  deriving (Eq, Ord, Show)

-- | Position of worms and tiles in the maze. (0,0) is at the top left
-- corner. The x axis grows to the right and the y axis grows downwards.
newtype Position = Pos (Int, Int)
  deriving (Eq, Ord, Show)

unPos :: Position -> (Int, Int)
unPos (Pos c) = c

-- | One of the four compas directions. In a (x, y) coordinate system, going
-- `South` means increasing y component, going `East` means increasing x
-- component, `North` and `East` go the opposite ways respectively.
data Direction
  = North
  | East
  | South
  | West
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Grid tiles
data Tile
  = Dirt -- ^ `Dirt` tiles are unvisited by any worms
  | Empty -- ^ `Empty` tiles have been excavated by a `Worm`. An `Empty` tile
    -- encountered by a worm will kill it.
  | Wall -- ^ `Wall` tiles are created next to `Empty` tiles created by a
    -- `Worm`. There is a chance a worm could burrow through it, but if not
    -- running into one will kill the `Worm`
  | Solid TileAnnotation -- ^ `Solid` tiles will always remain `Solid`. Running
    -- into one will kill a `Worm`
  deriving (Show, Eq, Ord)

-- | Annotations on `Solid` tiles. Solid tiles marked `Entry` or `Exit` will
-- guaranteed be connected to an `Empty` tile
data TileAnnotation
  = Edge -- ^ The solid tiles around the edge of the grid should be marked `Edge`
  | Internal -- ^ Tiles marked `Internal` are inside the grid, unlike `Edge` tiles
  | Entry -- ^ A maze entrance tile
  | Exit -- ^ A maze exit tile
  deriving (Show, Eq, Ord)


type GridCoord = (Int, Int)
type GridBounds = (GridCoord, GridCoord)

-- | Creates a `BurrowState` with a grid of the given internal size and using
-- default weights. The grid will have two extra rows and columns to provide
-- edge tiles.
defaultBurrowState :: (Int, Int) -> ST s (BurrowState s)
defaultBurrowState size = do
  grid <- newGrid size
  return BS {..}
  where worms = S.empty
        breachWallChance = 0.03
        turnChance = 0.70
        spawnChance = 0.20

-- Predicate determining if a wall should be breached
breachWall :: (RandomGen g)
           => Double -- ^ Breach chance
           -> g      -- ^ RNG
           -> Bool   -- ^ Should breach
breachWall breachChance g = fst (random g) < breachChance 

-- | Randomly turns depending on the given turn chance. If not turning will
-- return current direction, if turning it will prefer to return a random
-- direction in the prefered list. If the prefered list is empty it choses any
-- direction at random.
turn :: (RandomGen g)
     => Double -- ^ Turn chance
     -> g -- ^ RNG
     -> Direction -- ^ Current direction
     -> [Direction] -- ^ Prefered directions to turn to
     -> Direction -- ^ New direction
turn turnChance g currentDir preferedDirs
  = if r < turnChance then
       chooseL g1 dirs
     else
       currentDir
  where (r, g1) = random g
        dirs = if not (null preferedDirs) then
                 preferedDirs
               else
                 directions

-- | Spawns a worm at random at the given position depending on the given spawn
-- chance. The worm will face in one of the given directions at random.
spawnWorm :: (RandomGen g)
          => Double -- ^ Spawn chance
          -> g -- ^ RNG
          -> Position -- ^ Spawn position
          -> [Direction] -- ^ Possible spawn directions
          -> Maybe Worm -- ^ Spawned worm (or not)
spawnWorm spawnChance g pos dirs =
  let (r, g1) = random g
  in if not (null dirs) && r < spawnChance then
       Just (Worm pos (chooseL g1 dirs))
     else
       Nothing

-- | Creates a new grid with interior size (x, y). Adds two columns and two rows
-- for the edges. Valid indexes are from (0,0) to (x+1, y+1). The edges are
-- filled with `Solid Edge` and the interior tiles filled with `Dirt`
newGrid :: (Int, Int) -- ^ Interior size of the grid 
        -> ST s (Grid s)
newGrid (x, y) = do
  arr <- newArray ((0,0), (x+1,y+1)) Dirt
  forM_ [0..x+1] \i -> do
    writeArray arr (i, 0) (Solid Edge)
    writeArray arr (i, y+1) (Solid Edge)
  forM_ [0..y+1] \i -> do
    writeArray arr (0, i) (Solid Edge)
    writeArray arr (x+1, i) (Solid Edge)
  return arr

-- | Runs one step of the algorithm.
--
-- Each step mutates the underlying grid and produces a new set of worms
--
-- * If there are no live worms and no dirt tiles, we are done.
-- * A random worm is chosen from the set of live worms
-- * If the tile in front of the worm is not a dirt tile it could die
-- ** It always dies on empty or solid tiles
-- ** It has a chance to breach (move into) wall tiles
-- * The worm moves forward and the tile it moves into becomes empty
-- * The worm has a chance to turn, prefering to turn towards dirt tiles
-- * There is a chance a new worm will spawn facing a dirt tile other than the
--tile the other worm is facing.
-- * Neighboring dirt tiles are checked to avoid creating either a 2x2 empty
--room or a 2x2 wall square. If the neighboring tile has 3 empty neighbors in
--turn it is marked `Solid`, and if it has 3 neighboring `Wall`s or `Solid`
--tiles it remains dirt. Otherwise neighboring dirt tiles are marked `Wall`
--
--Finally the updated set of worms is returned
step :: forall s g. (RandomGen g) => g -> BurrowState s -> ST s (Maybe (S.Set Worm))
step g BS{..} =
  if S.null worms then
    return Nothing
  else do
    tile <- getTileInFront oldWorm grid
    case tile of
      Solid _ -> return deadWormSet
      Empty   -> return deadWormSet

      Wall    -> if not (breachWall breachWallChance dieG) then
                   return deadWormSet
                 else
                   getDirtDirections >>= updateGrid

      Dirt    -> getDirtDirections >>= updateGrid
    
  where wormG:dieG:turnG:spawnG:_ = splits g
  
        wormIx = fst $ randomR (0, S.size worms - 1) wormG
        oldWorm@(Worm oldPos oldDir) = S.elemAt wormIx worms
        noWormSet = worms \\ S.singleton oldWorm
        deadWormSet = Just noWormSet
        
        newPos@(Pos (x, y)) = moveInDir oldPos oldDir -- TODO bounds checking?

        getDirtDirections :: ST s [Direction]
        getDirtDirections = do
          neighborTiles <- mapM (readArray grid . unPos) (neighbors newPos)
          let dirtDirections = map (direction newPos . fst)
                               . filter ((==Dirt).snd)
                               . zip (neighbors newPos)
                               $ neighborTiles
          return dirtDirections

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
            newDir = turn turnChance turnG oldDir preferedDirs
            newWorm = Worm newPos newDir
            spawnedWorm = spawnWorm
                          spawnChance
                          wormG
                          newPos
                          (filter (/= newDir) preferedDirs)
        
            movedWormSet = S.insert newWorm noWormSet
            spawnedWormSet = Just case spawnedWorm of
                               Nothing -> movedWormSet
                               Just w -> S.insert w movedWormSet

-- | Fills the tile at the given `Position` (assumed to already be `Dirt`) while
-- avoiding 2x2 square of either `Empty` tiles or `Wall`/`Solid` tiles.
--
--If it has 3 neighboring `Empty` tiles it's marked `Solid`, if it was 3
-- neighboring `Wall`/`Solid` tiles it's marked `Dirt` (which is should already
-- be), otherwise it's marked `Wall`
fillAvoidingSquare :: STArray s (Int, Int) Tile -> Position -> ST s ()
fillAvoidingSquare grid pos = do
  avoidedSquares <- forM (squaresWith pos) $ \square -> do
    -- Look for 3 tiles in a square that are either Empty or Wall/Solid
    squareTiles <- map (\(t:_) -> if t then Empty else Wall)
                   . filter ((==3).length)
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

-- | Given the initial state, runs the burrow algorithm and returns the
-- generated maze

-- TODO Get rid of the ST argument and use some other representation of the
-- initial grid
runBurrow :: (RandomGen g)
          => g
          -> (forall s. ST s (BurrowState s)) -- ^
          -> IGrid
runBurrow g getBs = runST $ do
  bs <- getBs
  doBurrow g bs
  freeze (grid bs)

-- | Runs the burrow algorithm with default parameters on a grid of size (x,y)
runDefaultBurrow :: (RandomGen g) => g -> (Int, Int) -> IGrid
runDefaultBurrow g (x, y) = runBurrow g1 getBs
  where
    (g1:g2:_) = splits g
    getBs = do
      bs <- defaultBurrowState (x, y)
      return bs{worms= S.singleton (randomWorm g2 ( (1, 1)
                                                  , (x , y)))}

-- | Loops the steps until the algorithm is done
doBurrow :: (RandomGen g) => g -> BurrowState s -> ST s ()
doBurrow g bs = do
  newWorms <- step g1 bs
  case newWorms of
    Nothing -> return ()
    Just nw -> doBurrow g2 bs{worms = nw}
  where
    (g1:g2:_) = splits g

-- | Creates a random worm within the given grid bounds
randomWorm :: (RandomGen g) => g -> GridBounds -> Worm
randomWorm g ((lowX, lowY), (highX, highY))
  = Worm (Pos (fst (randomR (lowX, highX) g1)
              ,fst (randomR (lowY, highY) g2)))
         (chooseL g3 directions)
  where (g1:g2:g3:_) = splits g

-- | Determines if the position is directly in front of the worm
isTileInFrontOf :: Worm -> Position -> Bool
isTileInFrontOf (Worm wormPos wormDir) pos
  = pos == moveInDir wormPos wormDir

-- | Looks up the tile in front of the worm
getTileInFront :: Worm -> STArray s (Int, Int) Tile -> ST s Tile
getTileInFront (Worm pos dir) grid = do
  bounds <- getBounds grid
  let (Pos (x, y)) = moveInDir pos dir
  readArray grid (x, y)

-- | Maps directions to their opposites
opposite :: Direction -> Direction
opposite North = South
opposite South = North
opposite East = West
opposite West = East

-- | List of all directions
directions :: [Direction]
directions = [North, South, East, West]

-- | List of all neighboring positions. Does not include diagonal neigbors
neighbors :: Position -> [Position]
neighbors (Pos (x, y)) =
  [ Pos (x-1, y)
  , Pos (x+1, y)
  , Pos (x, y-1)
  , Pos (x, y+1)
  ]

-- | Direction from a given position to another position. Only well defined for
-- positions on the same horizontal or vertical position.
direction :: Position -- ^ from
          -> Position -- ^ to
          -> Direction
direction (Pos (x1, y1)) (Pos (x2, y2))
  | x1 > x2 = West
  | x2 > x1 = East
  | y1 > y2 = South
  | y2 > y1 = North

-- | The four positions forming the square with the given 'Position' in the
-- upper left corner (i.e. the "lowest" positon)
squareAt :: Position -> [Position]
squareAt (Pos (x,y)) = [Pos (x+dx, y+dy) | dx <- [0,1], dy <- [0,1]]

-- | All four squares that contain the given 'Position'
squaresWith :: Position -> [[Position]]
squaresWith (Pos (x,y)) = map squareAt . squareAt $ Pos (x-1, y-1)

-- | The position you get to if you move one step from the given 'Position' in
-- the given 'Direction'
moveInDir :: Position -> Direction -> Position
moveInDir (Pos (x, y)) dir = case dir of
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
        (_,(x,y)) = bounds arr
        builder = mconcat [mconcat chars <> TB.singleton '\n'
                          | x' <- [0..x]
                          , let chars = [TB.singleton . tileToChar $ arr ! (x', y')
                                        | y' <- [0..y]]]


printGrid :: IGrid -> IO ()
printGrid arr = LTIO.putStr (showGrid arr)

-- | Choose a random element out of a list
chooseL :: RandomGen g => g -> [a] -> a
chooseL g list = list !! index
  where index = fst (randomR (0, len) g)
        len = (length list) - 1

-- | Infinite list of generators
splits :: RandomGen g => g -> [g]
splits g = let (g', gs) = split g
           in g':splits gs
