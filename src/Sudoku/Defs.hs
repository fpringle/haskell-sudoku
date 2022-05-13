module Sudoku.Defs where

import Data.List

-- type definition
type Sudoku = [[Int]]

type Pos = (Int, Int)

(!!!) :: Sudoku -> Pos -> Int
s !!! (i, j) = s !! i !! j

blank :: Sudoku
blank = replicate 9 $ replicate 9 0
