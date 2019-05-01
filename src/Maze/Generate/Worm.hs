{-# LANGUAGE LambdaCase #-}
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
  --of them. The walls and empty tiles in the final maze are mostly 1 tile wide
  --with as few larger open rooms or columns as possible.
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
    BurrowConfig(..)
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
  , defaultBurrowConfig
  , newGrid

  , runDefaultBurrow
  , runWithRandomWorm
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

import Maze.Generate.Worm.Internal

import System.Random

import Control.Monad
import Control.Monad.ST

import Data.Array.ST
import Data.Array.IArray

import qualified Data.Set as S
import Data.Set ((\\))
import Data.List (sort, group)

import Data.Maybe

-- | Creates a `BurrowConfig` with a grid of the given internal size and using
-- default weights. The grid will have two extra rows and columns to provide
-- edge tiles.
defaultBurrowConfig :: (Int, Int) -> BurrowConfig
defaultBurrowConfig size = BurrowConfig {..}
  where gridSize = size
        worms = S.empty
        breachWallChance = 0.03
        turnChance = 0.70
        spawnChance = 0.20
        avoidSquares = True
        respawnWorms = True
        fillLeftoverDirt = True
        randomWormSelection = True

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
          -> Int -- ^ Priority
          -> Position -- ^ Spawn position
          -> [Direction] -- ^ Possible spawn directions
          -> Maybe Worm -- ^ Spawned worm (or not)
spawnWorm spawnChance g pri pos dirs =
  let (r, g1) = random g
  in if not (null dirs) && r < spawnChance then
       Just (Worm pri pos (chooseL g1 dirs))
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
--tiles it remains dirt. Otherwise neighboring dirt tiles are marked
--`Wall`. Note that this does not guarantee that there won't ever be a 2x2
--square created
--
--Finally the updated set of worms is returned
step :: forall s g. (RandomGen g)
     => g
     -> BurrowConfig
     -> Grid s
     -> ST s (Maybe (S.Set Worm))
step g BurrowConfig{..} grid =
  if | S.null worms && respawnWorms -> do
         (min, max) <- getBounds grid
         let startPos = fst $ randomR (Pos min, Pos max) g
         wormParams <- getNewWormParameters grid (startPos)
         case wormParams of
           Nothing -> return Nothing
           Just (pos, dir) -> do
             writeArray grid (unPos pos) Empty
             return $ Just (S.singleton (Worm 0 pos dir))
             
     | S.null worms && not respawnWorms ->
         return Nothing
         
     | otherwise -> do
         tile <- getTileInFront grid oldWorm
         case tile of
           Solid _ -> return deadWormSet
           Empty   -> return deadWormSet

           Wall    -> if not (breachWall breachWallChance dieG) then
                        return deadWormSet
                      else moveWorm

           Dirt    -> moveWorm
                                       
    
  where wormG:dieG:turnG:spawnG:_ = splits g
  
        wormIx | randomWormSelection = fst $ randomR (0, S.size worms - 1) wormG
               | otherwise = 0
        oldWorm@(Worm pri oldPos oldDir) = S.elemAt wormIx worms
        noWormSet = worms \\ S.singleton oldWorm
        deadWormSet = Just (S.mapMonotonic
                            (\(Worm pri pos dir) -> Worm (pri-1) pos dir)
                            noWormSet)
        
        newPos@(Pos (x, y)) = moveInDir oldPos oldDir -- TODO bounds checking?

        moveWorm = do dirtDirections <- getDirtDirections grid newPos
                      tileInFront <- getTileInFront grid (Worm 0 newPos oldDir)
                      updateGrid dirtDirections tileInFront

        updateGrid :: [Direction] -> Tile -> ST s (Maybe (S.Set Worm))
        updateGrid dirtDirections tileInFront = do
          writeArray grid (x, y) Empty
          forM_ (neighbors newPos) \neighbor ->
            unless (isTileInFrontOf newWorm neighbor
                    || maybe False (flip isTileInFrontOf neighbor) spawnedWorm) $ do
              tile <- readArray grid (unPos neighbor)
              case tile of
                Dirt -> if avoidSquares then
                          fillAvoidingSquare grid neighbor
                        else
                          fillSquare grid neighbor
                _    -> return ()
          return spawnedWormSet
          
          where
            preferedDirs = if null dirtDirections then directions else dirtDirections
            newDir = turn (if tileInFront == Dirt then turnChance else 1) turnG oldDir preferedDirs
            newWorm = Worm pri newPos newDir
            spawnedWorm = spawnWorm
                          spawnChance
                          wormG
                          (S.size worms)
                          newPos
                          (filter (/= newDir) preferedDirs)
        
            movedWormSet = S.insert newWorm noWormSet
            spawnedWormSet = Just case spawnedWorm of
                               Nothing -> movedWormSet
                               Just w -> S.insert w movedWormSet

-- | The list of 'Direction's that point to tiles with 'Dirt' from the given 'Position'
getDirtDirections :: Grid s -> Position -> ST s [Direction]
getDirtDirections grid pos = do
  neighborTiles <- mapM (readArray grid . unPos) (neighbors pos)
  let dirtDirections = map (direction pos . fst)
                       . filter ((==Dirt).snd)
                       . zip (neighbors pos)
                       $ neighborTiles
  return dirtDirections


-- | Fills the 'Tile' at the given 'Position' (assumed to already be 'Dirt') while
-- avoiding 2x2 square of either 'Empty' tiles or 'Wall'/'Solid' tiles.
--
-- If the tile wasn't marked to avoid a square it is marked as 'Wall'
-- 
-- There is still a chance 2x2 squares can appear
fillAvoidingSquare :: Grid s -> Position -> ST s ()
fillAvoidingSquare grid pos = do
  avoidedSquares <- avoidSquare grid pos
  if avoidedSquares then return ()
    else fillSquare grid pos

-- | Marks the position as a 'Wall'
fillSquare :: Grid s -> Position -> ST s ()
fillSquare grid pos = writeArray grid (unPos pos) Wall

-- | Avoids 2x2 squares of a single 'Tile' type by checking the squares the given
-- 'Position' is a part of if they either contain 3 'Empty' tiles or 3
-- 'Wall'/'Solid' tiles already. The tile at the given position is assumed to be
-- 'Dirt'
--
-- If it has 3 neighboring 'Empty' tiles it's marked 'Solid', if it was 3
-- neighboring 'Wall'/'Solid' tiles it's marked 'Empty'
avoidSquare :: Grid s -> Position -> ST s Bool
avoidSquare grid pos =
  or <$> forM (squaresWith pos) \square -> do
  squareTiles <- map (\(t:_) -> if t then Empty else Wall)
                 . filter ((==3).length)
                 . group
                 . sort
                 . map (==Empty)
                 . filter (/=Dirt)
                 <$> mapM (readArray grid . unPos) square
  forM_ squareTiles $ \case
    Empty -> writeArray grid (unPos pos) (Solid Internal)
    Wall  -> writeArray grid (unPos pos) Empty
    _     -> return ()
  return . not . null $ squareTiles

-- | Returns the position of all 'Tile's that match the given predicate
getTilesOfType :: Grid s -> (Tile -> Bool) -> ST s [Position]
getTilesOfType grid p = do
  ps <- range <$> getBounds grid
  map Pos <$> filterM (\t -> p <$> readArray grid t) ps

-- | Returns all the neighbors that match the given predicate. Position must not
-- be an edge position
getNeighborsOfType :: Grid s -> Position -> (Tile -> Bool) -> ST s [Position]
getNeighborsOfType grid pos p = map Pos <$> filterM isType nbs
  where isType t = p <$> readArray grid t
        nbs = map unPos $ neighbors pos

-- | Returns all the neighbors of the given 'Position' that match the given
-- predicate, as well as the direction from the neighbor to the given position
-- (i.e. @moveDir neighbor direction == position@)
getNeighborsPointingTo :: Grid s
                       -> Position
                       -> (Tile -> Bool)
                       -> ST s [(Position, Direction)]
getNeighborsPointingTo grid tile p = do
  nbs <- getNeighborsOfType grid tile p
  return [(n, direction n tile) | n <- nbs]

-- | Returns the parameters for a new worm starting at either an empty tile or
-- a wall tile and looking at a dirt tile.
getNewWormParameters :: Grid s -> Position -> ST s (Maybe (Position, Direction))
getNewWormParameters grid start = do
  bnds <- getBounds grid
  go (spiral start bnds)
  where
    go [] = return Nothing
    go (pos:ps) = do
      t <- readArray grid (unPos pos)
      if t /= Dirt then
        go ps
        else do
        goodTile <- listToMaybe <$> getNeighborsPointingTo grid pos
                    ((||) <$> (==Wall) <*> (==Empty))
        if isNothing goodTile then
          go ps
          else return goodTile

-- | Given the initial state, runs the burrow algorithm and returns the
-- generated maze

runBurrow :: (RandomGen g)
          => g
          -> BurrowConfig
          -> IGrid
runBurrow g conf = runST $ do
  grid <- newGrid (gridSize conf)
  doBurrow g conf grid
  freeze grid

-- | Runs the burrow algorithm with default parameters on a grid of size (x,y)
runDefaultBurrow :: (RandomGen g) => g -> (Int, Int) -> IGrid
runDefaultBurrow g size
  = runWithRandomWorm g (defaultBurrowConfig size)

-- | Runs the burrow algorithm with a single random worm
runWithRandomWorm :: (RandomGen g)
                  => g
                  -> BurrowConfig
                  -> IGrid
runWithRandomWorm g conf =
  let (g1, g2) = split g
      Just worm = randomWormInBounds g1 ((1, 1), gridSize conf)
  in runBurrow g2 conf{worms = S.singleton worm}

-- | Loops the steps until the algorithm is done
doBurrow :: (RandomGen g) => g -> BurrowConfig -> Grid s -> ST s ()
doBurrow g conf grid = do
  newWorms <- step g1 conf grid
  case newWorms of
    Just nw -> doBurrow g2 conf{worms = nw} grid
    Nothing -> if fillLeftoverDirt conf
               then map unPos <$> getTilesOfType grid (==Dirt)
                    >>= mapM_ (\p -> writeArray grid p Wall)
               else return ()
  where
    (g1:g2:_) = splits g

-- | Creates a random worm within the given grid bounds
randomWormInBounds :: (RandomGen g) => g -> GridBounds -> Maybe Worm
randomWormInBounds g bnds = randomWorm g
                            [ (Pos pos, dir)
                            | pos <- range bnds
                            , dir <- directions]

randomWorm :: RandomGen g => g -> [(Position, Direction)] -> Maybe Worm
randomWorm _ [] = Nothing
randomWorm g opts = Just . uncurry (Worm 0) . chooseL g $ opts

-- | Determines if the position is directly in front of the worm
isTileInFrontOf :: Worm -> Position -> Bool
isTileInFrontOf (Worm _ wormPos wormDir) pos
  = pos == moveInDir wormPos wormDir

-- | Looks up the tile in front of the worm
getTileInFront :: Grid s -> Worm -> ST s Tile
getTileInFront grid (Worm _ pos dir) = do
  bounds <- getBounds grid
  let (Pos (x, y)) = moveInDir pos dir
  readArray grid (x, y)
