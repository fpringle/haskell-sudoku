module Sudoku.Validity where

import Sudoku.Defs
import Sudoku.Util

-- checks

-- check the grid has the right dimensions
isRightSize :: Sudoku -> Bool
isRightSize (Sudoku s) = (length s == 9) && all (\r -> length r == 9) s

noDuplicates :: [Int] -> Bool
noDuplicates [] = True
noDuplicates (x:xs) = (x == 0 || not (elem x xs)) && noDuplicates xs

_isValidFunc :: (Sudoku -> Int -> [Int]) -> Sudoku -> Bool
_isValidFunc f s = all (noDuplicates . f s) [0 .. 8]

isValidRows :: Sudoku -> Bool
isValidRows = _isValidFunc getRow

isValidCols :: Sudoku -> Bool
isValidCols = _isValidFunc getCol

isValidBoxes :: Sudoku -> Bool
isValidBoxes = _isValidFunc getBoxFlat

isValid :: Sudoku -> Bool
isValid s = isRightSize s && isValidRows s && isValidCols s && isValidBoxes s

isSolved :: Sudoku -> Bool
isSolved (Sudoku s) = isValid (Sudoku s) && all (not . elem 0) s
