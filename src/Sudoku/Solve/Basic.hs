module Sudoku.Solve.Basic where

import Data.List

import Sudoku.Defs
import Sudoku.Util
import Sudoku.Validity


type SudokuWithOptions = [[[Int]]]

setOptions :: SudokuWithOptions -> Pos -> [Int] -> SudokuWithOptions
setOptions = placeInGrid

genInitialOptions :: Sudoku -> SudokuWithOptions
genInitialOptions s = foldr helper init allSquaresFlat
  where
    init :: SudokuWithOptions
    init = [[[] | j <- [0 .. 8]] | i <- [0 .. 8]]

    helper :: Pos -> SudokuWithOptions -> SudokuWithOptions
    helper pos cur
      | s !!! pos > 0   = setOptions cur pos [s !!! pos]
      | otherwise       = setOptions cur pos $ getOptions s pos

    getOptions :: Sudoku -> Pos -> [Int]
    getOptions grid (i, j) = foldr delete [1 .. 9] (row ++ col ++ box)
      where
        row = getRow grid i
        col = getCol grid j
        box = getBoxFlat grid $ getBoxFromCoord (i, j)

optionsToNormal :: SudokuWithOptions -> Sudoku
optionsToNormal = map (map (\x -> if length x == 1 then head x else 0))

stepBasic :: SudokuWithOptions -> SudokuWithOptions
stepBasic s = foldr helper s allSquaresFlat
  where
    -- if the square at pos only has one option x, eliminate all occurrences
    -- of x from its row, column and box
    helper :: Pos -> SudokuWithOptions -> SudokuWithOptions
    helper pos grid
      | (length $ grid !!! pos) /= 1    = grid
      | otherwise                       = reduceOptions pos (head $ grid !!! pos) grid

    -- eliminate occurrences
    reduceOptions :: Pos -> Int -> SudokuWithOptions -> SudokuWithOptions
    reduceOptions (i1,j1) x g = map (map helper2) allSquares
      where
        helper2 (i2, j2) =
          let ati2j2 = g !!! (i2, j2)
          in if similar (i1, j1) (i2, j2)
             then delete x ati2j2
             else ati2j2
