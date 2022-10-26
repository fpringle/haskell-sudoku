{-
Copyright (c) 2022, Frederick Pringle
All rights reserved.

This source code is licensed under the BSD-style license found in the
LICENSE file in the root directory of this source tree.
-}

module Sudoku.Validity
  (
    {- * Grid predicates
    -}
    isValid
    {- ** solution predicates
    -}
    , isSolved
  ) where

import Sudoku.Types
import Sudoku.Util

-- checks

{- | check that a grid has the right dimensions
-}
isRightSize :: Sudoku -> Bool
isRightSize (Grid s) = (length s == 9) && all (\r -> length r == 9) s

{- | check that a list has no duplicate elements
-}
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

{- | check that a grid is valid
-}
isValid :: Sudoku -> Bool
isValid s = isRightSize s && isValidRows s && isValidCols s && isValidBoxes s

{- | check that a grid is solved, i.e. complete and valid
-}
isSolved :: Sudoku -> Bool
isSolved (Grid s) = isValid (Grid s) && all (not . elem 0) s
