module Sudoku.Defs where

import Data.List

{- | Abstract type representing a 2x2 grid.
-}
data Grid a = Grid [[a]]
  deriving (Show, Eq)

{- | The Functor instance for Grid lets us map over a 2x2 grid in the natural way.
-}
instance Functor Grid where
  fmap f (Grid xs) = Grid $ fmap (fmap f) xs

{- | Sudoku is a 2x2 list representing the contents of a partially- or
totally-filled sudoku grid. A blank space is represented by a 0.
-}
type Sudoku = Grid Int

{- | Pos is an (Int, Int) tuple representing the position of a cell in a sudoku grid.
-}
type Pos = (Int, Int)

{- | get the cell value at a position in the grid.
-}
(!!!) :: Sudoku -> Pos -> Int
(Grid s) !!! (i, j) = s !! i !! j

infixl 9 !!!

{- | a blank sudoku grid
-}
blank :: Sudoku
blank = Grid $ replicate 9 $ replicate 9 0
