{-
Copyright (c) 2022, Frederick Pringle
All rights reserved.

This source code is licensed under the BSD-style license found in the
LICENSE file in the root directory of this source tree.
-}

-- | Functions to check the validity and correctness of a Sudoku grid.

module Sudoku.Validity (
    {- * Grid predicates
    -}
    isValid
    , isRightSize
    -- , isValidFunc
    , isValidRows
    , isValidCols
    , isValidBoxes

    {- * Solution predicates
    -}
    , isSolved
  ) where

import Sudoku.Types
import Sudoku.Util

-- | check that a grid has the right dimensions
isRightSize :: Sudoku -> Bool
isRightSize (Grid s) = (length s == 9) && all (\r -> length r == 9) s

-- | check that a list has no duplicate elements
noDuplicates :: [Int] -> Bool
noDuplicates [] = True
noDuplicates (x:xs) = (x == 0 || notElem x xs) && noDuplicates xs

isValidFunc :: (Sudoku -> Int -> [Int]) -> Sudoku -> Bool
isValidFunc f s = all (noDuplicates . f s) [0 .. 8]

-- | check that a grid has no rows with duplicate elements
isValidRows :: Sudoku -> Bool
isValidRows = isValidFunc getRow

-- | check that a grid has no cols with duplicate elements
isValidCols :: Sudoku -> Bool
isValidCols = isValidFunc getCol

-- | check that a grid has no boxes with duplicate elements
isValidBoxes :: Sudoku -> Bool
isValidBoxes = isValidFunc getBoxFlat

-- | check that a grid is valid
isValid :: Sudoku -> Bool
isValid s = isRightSize s && isValidRows s && isValidCols s && isValidBoxes s

-- | check that a grid is solved, i.e. complete and valid
isSolved :: Sudoku -> Bool
isSolved (Grid s) = isValid (Grid s) && all (notElem 0) s
