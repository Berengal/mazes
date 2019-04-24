{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Maze.Generate where

import Control.Monad.Random
import Control.Monad.ST

import Data.Array.MArray
import Data.Array.ST
import qualified Data.Array.Unboxed as U
import qualified Data.Array.IO as IOA

import Data.Foldable

type GridCoord = (Int, Int)
type GridBounds = (GridCoord, GridCoord)

-- | The edge at coord (x,y) is the edge between the node at (x,y) and either
-- (x,y+1) if direction is horizontal or (x+1, y) if direction is vertical
type EdgeCoord = (Int, Int, Dir)

data Dir = Hor | Vert
  deriving (Show, Eq, Ord, Enum)

neighbors :: GridCoord -> GridBounds -> [GridCoord]
neighbors (x, y) bounds =
  let ns = [ (x-1, y)
           , (x+1, y)
           , (x, y-1)
           , (x, y+1)
           ]
  in filter (inRange bounds) ns

edges :: GridCoord -> GridBounds -> [EdgeCoord]
edges (x, y) bounds = map snd . filter (inRange bounds . fst) $ ns
  where ns = [ ((x-1, y), (x-1, y, Hor))
             , ((x+1, y), (x, y, Hor))
             , ((x, y-1), (x, y-1, Vert))
             , ((x, y+1), (x, y, Vert))
             ]



unvisitedNeighbors :: _ => GridCoord -> _ -> _ [GridCoord]
unvisitedNeighbors node visitedArr = do
  bounds <- getBounds visitedArr
  filterM (fmap not . readArray visitedArr) (neighbors node bounds)


shuffleList :: RandomGen g => [e] -> g -> ST s [e]
shuffleList list g = do
  let l = length list
  arr <- newListArray (0, (l-1)) list :: ST s (STArray s Int _)
  shuffle arr g
  getElems arr

shuffle :: (MArray a e m, RandomGen g, _) => a i e -> g -> m ()
shuffle arr = evalRandT do
  let swap i j = do
        a <- readArray arr i
        b <- readArray arr j
        writeArray arr i b
        writeArray arr j a
  (start, end) <- lift $ getBounds arr
  forM_ [start..end-1] \i -> do
    j <- getRandomR (i, end)
    lift $ swap i j

-- shuffledArray :: RandomGen p => p -> U.UArray Int Int
-- shuffledArray g = runSTUArray do
--   arr <- newListArray (0,10) [0..10] :: ST s (STUArray s Int Int)
--   shuffle arr g
--   return arr

-- testRandomness = do
--   g <- newStdGen
--   let bounds = (1,1000) :: (Int, Int)
--       rands = randomRs bounds g
--   arr <- newArray bounds 0 :: IO (IOA.IOUArray Int Int)
--   forM_ (take (10^6) rands) \n -> do
--     count <- readArray arr n
--     writeArray arr n (count + 1)
--   a <- U.amap (fromIntegral @Int @Double) <$> freeze arr :: _ (U.UArray _ _)
--   let foldArr f x = foldl' f x (U.elems a)
--       avg = foldArr (+) 0 / size
--       stddev = foldArr (\c n -> c + (n - avg)^2) 0 / size
--       size = fromIntegral . rangeSize $ U.bounds a
--   print (avg, sqrt stddev)
