{-# LANGUAGE ViewPatterns #-}
module Maze.Picture
  ( gridToImage
  , gridToScaledImage
  ) where

import Codec.Picture

import Maze.Generate.Worm (IGrid, GridCoord, Tile(..), TileAnnotation(..))
import Data.Array

gridToImage :: IGrid -> Image Pixel8
gridToImage grid = gridToScaledImage grid 1

gridToScaledImage :: IGrid -> Int -> Image Pixel8
gridToScaledImage grid scale = generateImage getPixel width height
  where
    ((minX, minY), (maxX, maxY)) = bounds grid
    width = (maxX - minX + 1) * scale
    height = (maxY - minY + 1) * scale
    getPixel
      ((`div` scale) -> x)
      ((`div` scale) -> y) = tileToPixel (grid ! (x, y))

tileToPixel :: Tile -> Pixel8
tileToPixel Empty = maxBound
tileToPixel Wall = minBound
tileToPixel (Solid _) = minBound
tileToPixel Dirt = maxBound `div` 2

