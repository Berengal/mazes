module Maze.Generate.Worm.Internal where

import System.Random

import Data.Array.ST
import Data.Array.IArray

import qualified Data.Set as S

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as TB

import qualified Data.Text.Lazy.IO as LTIO

-- | The 'BurrowConfig' contains the state used for running the algorithm

data BurrowConfig = BurrowConfig
  { gridSize :: (Int, Int) -- ^ The internal size of the grid
     
--     , grid :: Grid s -- ^ The maze grid  
  , worms :: S.Set Worm -- ^ The set of active worms on the grid
  , avoidSquares :: Bool -- ^ Use extra logic to avoid 2x2 squares
  , respawnWorms :: Bool -- ^ Spawn new worms if there still dirt tiles with no
                   -- live worms
  , fillLeftoverDirt :: Bool -- ^ Turn any dirt tiles into walls after the
                        -- algorithm is done
  , randomWormSelection :: Bool -- ^ Select a new random worm after each
                           -- generation step
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

-- | Worms burrow through the maze. Each has a priority, a position and a direction
data Worm = Worm Int Position Direction
  deriving (Eq, Show)

instance Ord Worm where
  compare (Worm pri1 pos1 dir1) (Worm pri2 pos2 dir2)
    =  compare pri1 pri2
    <> compare pos1 pos2
    <> compare dir1 dir2

-- | Position of worms and tiles in the maze. (0,0) is at the top left
-- corner. The x axis grows to the right and the y axis grows downwards.
newtype Position = Pos (Int, Int)
  deriving (Eq, Ord, Show)

unPos :: Position -> (Int, Int)
unPos (Pos c) = c

instance Random Position where
  random g = let (x, g') = random g
                 (y, g'') = random g'
             in (Pos (x,y), g'')

  randomR (Pos (minX, minY), Pos (maxX, maxY)) g
    = let (x, g') = randomR (minX, maxX) g
          (y, g'') = randomR (minY, maxY) g'
      in (Pos (x,y), g'')               

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

-- | Outwards spiral starting at the given 'Position'. Center of the spiral can
-- be outside of the given 'GridBounds' but only positions inside will be returned.
spiral :: Position -> GridBounds -> [Position]
spiral (Pos (x,y)) bnds@((minX, minY), (maxX, maxY))
  = map Pos . filter (inRange bnds) $ (x,y) : spiral' 1
  where
    iterations = maximum . map abs $ [x - minX, x - maxX, y - minY, y - maxY]
    spiral' n | n > iterations = []
    spiral' n = [(x', startY) | x' <- [startX .. endX]]
                ++ [(endX, y') | y' <- [startY+1 .. endY]]
                ++ [(x', endY) | x' <- [endX-1, endX-2 .. startX]]
                ++ [(startX, y') | y' <- [endY-1, endY-2 .. startY+1]]
                ++ spiral' (n+1)
      where startX = x - n
            endX   = startX + (2*n)
            startY = y - n
            endY   = startY + (2*n)
