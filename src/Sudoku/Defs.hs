module Sudoku.Defs where

import Data.List

-- type definition
data Sudoku = Sudoku [[Int]]
  deriving (Show, Eq)

type Pos = (Int, Int)

(!!!) :: Sudoku -> Pos -> Int
(Sudoku s) !!! (i, j) = s !! i !! j

infixl 9 !!!


blank :: Sudoku
blank = Sudoku $ replicate 9 $ replicate 9 0
