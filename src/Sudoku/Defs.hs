module Sudoku.Defs where

import Data.List

-- type definition
type Sudoku = [[Int]]

type Pos = (Int, Int)

(!!!) :: [[a]] -> Pos -> a
s !!! (i, j) = s !! i !! j

infixl 9 !!!


blank :: Sudoku
blank = replicate 9 $ replicate 9 0
