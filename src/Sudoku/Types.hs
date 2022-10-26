{-
Copyright (c) 2022, Frederick Pringle
All rights reserved.

This source code is licensed under the BSD-style license found in the
LICENSE file in the root directory of this source tree.
-}

{- | Sudoku.Types defines the datatypes used by the library and the solver.
-}

module Sudoku.Types (
  {- * Grid and position types
   -}
  Grid (..)
  , Sudoku
  , Pos
  , blank

  {- * Functions on grids
  -}
  , (!!!)
  , getRow
  , getCol
  , getBoxCoords
  , getBoxCoordsFlat
  , getBox
  , getBoxFlat
  , getBoxFromCoord
  , allSquares
  , allSquaresFlat
  , mapRows
  , transposeGrid
  , place
  , similar
  , replaceValues
  ) where

import Data.List
import Data.Maybe (fromMaybe)

{- | Abstract type representing a 2-dimensional grid.
-}
newtype Grid a = Grid [[a]]
  deriving (Show, Eq)

{- | map over the rows of a 'Grid'
-}
mapRows :: ([a] -> [b]) -> Grid a -> Grid b
mapRows f (Grid xs) = Grid (map f xs)

{- | transpose a grid using 'transpose' from "Data.List"
-}
transposeGrid :: Grid a -> Grid a
transposeGrid (Grid grid) = Grid $ transpose grid

{- | The Functor instance for Grid lets us map over a 2D grid in the natural way.
-}
instance Functor Grid where
  fmap f (Grid xs) = Grid $ fmap (fmap f) xs

{- | Sudoku is a 2D 9x9 list representing the contents of a partially- or
totally-filled sudoku grid. A blank space is represented by a 0.
-}
type Sudoku = Grid Int

{- | Pos is an (Int, Int) tuple representing the position of a cell in a sudoku grid.
-}
type Pos = (Int, Int)

{- | get the cell value at a position in a grid.
-}
(!!!) :: Grid a -> Pos -> a
(Grid s) !!! (i, j) = s !! i !! j

infixl 9 !!!

{- | a blank sudoku grid
-}
blank :: Sudoku
blank = Grid $ replicate 9 $ replicate 9 0

{- | get a row of the grid
-}
getRow :: Sudoku -> Int -> [Int]
getRow (Grid s) = (!!) s

{- | get a column of the grid
-}
getCol :: Sudoku -> Int -> [Int]
getCol (Grid s) x = [r !! x | r <- s]

{- | get the coordinates of a box (ordered right to left, top to bottom) as a Grid
-}
getBoxCoords :: Int -> Grid Pos
getBoxCoords x =
  let
    i = x `div` 3
    j = x `mod` 3
    i1 = i * 3
    i2 = i1 + 2
    j1 = j * 3
    j2 = j1 + 2
  in
    Grid [[(row, col) | col <- [j1 .. j2]] | row <- [i1 .. i2]]

{- | get the coordinates of a box (ordered right to left, top to bottom) as a Grid
-}
getBoxCoordsFlat :: Int -> [Pos]
getBoxCoordsFlat x = let Grid xs = getBoxCoords x in concat xs

{- | get a box of the grid as a Grid
-}
getBox :: Grid a -> Int -> Grid a
getBox s = fmap (s !!!) . getBoxCoords

{- | get a box of the grid as a list
-}
getBoxFlat :: Grid a -> Int -> [a]
getBoxFlat s x = map (s !!!) $ getBoxCoordsFlat x

{- | get the index of the box a cell belongs to
-}
getBoxFromCoord :: Pos -> Int
getBoxFromCoord (i, j) = div i 3 * 3 + div j 3

{- | all cell positions as a 2D Grid
-}
allSquares :: Grid Pos
allSquares = Grid [[(i, j) | j <- [0 .. 8]] | i <- [0 .. 8]]

{- | all cell positions as a list
-}
allSquaresFlat :: [Pos]
allSquaresFlat = [(i, j) | i <- [0 .. 8], j <- [0 .. 8]]

{- | set an entry of a 2D grid
-}
place :: Grid a -> Pos -> a -> Grid a
place (Grid s) (i, j) n =
  let ith = s !! i
  in Grid (take i s ++ [take j ith ++ [n] ++ drop (j+1) ith] ++ drop (i+1) s)

{- | check if two positions are "similar", i.e. in the same row/box/col but NOT the same
-}
similar :: Pos -> Pos -> Bool
similar (i1, j1) (i2, j2) = (i1, j1) /= (i2, j2) && (
                                i1 == i2 ||
                                j1 == j2 ||
                                getBoxFromCoord (i1,j1) == getBoxFromCoord (i2,j2)
                                )

{- | given a grid and a list representing a map, replace all values of the grid
according to the map
-}
replaceValues :: (Show a, Eq a) => [(a, a)] -> Grid a -> Grid a
replaceValues replacements = fmap helper
  where helper x = fromMaybe errMsg maybeResult
          where errMsg = error ("bad lookup: " ++ show x ++ " (replacements: " ++ show replacements ++ ")")
                maybeResult = lookup x replacements
