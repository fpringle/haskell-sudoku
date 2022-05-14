module Sudoku.Defs where

import Data.List

{- | Sudoku represents a partially- or totally-filled sudoku grid
-}
data Sudoku
    = Sudoku
        [[Int]] {- ^ a 2x2 list representing the contents of the sudoku grid.
                    A blank space is represented by a 0.
                -}
    deriving (Show, Eq)

{- | Pos is an (Int, Int) tuple representing the position of a cell in a sudoku grid.
-}
type Pos = (Int, Int)

{- | get the cell value at a position in the grid.
-}
(!!!) :: Sudoku -> Pos -> Int
(Sudoku s) !!! (i, j) = s !! i !! j

infixl 9 !!!

{- | a blank sudoku grid
-}
blank :: Sudoku
blank = Sudoku $ replicate 9 $ replicate 9 0
