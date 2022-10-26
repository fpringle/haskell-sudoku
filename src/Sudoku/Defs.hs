{-
Copyright (c) 2022, Frederick Pringle
All rights reserved.

This source code is licensed under the BSD-style license found in the
LICENSE file in the root directory of this source tree.
-}

module Sudoku.Defs where

import Data.List

{- | Abstract type representing a 2-dimensional grid.
-}
data Grid a = Grid [[a]]
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
