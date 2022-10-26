{-
Copyright (c) 2022, Frederick Pringle
All rights reserved.

This source code is licensed under the BSD-style license found in the
LICENSE file in the root directory of this source tree.
-}

module Sudoku.Tests.Solve.Basic where


import Test.QuickCheck
import Test.QuickCheck.Instances.Tuple

import Sudoku.Types
import Sudoku.Util
import Sudoku.Validity
import Sudoku.Solve.Basic

import Sudoku.Tests.TestUtils


propGenInitialOptions :: Sudoku -> Property
propGenInitialOptions s = conjoin $ map helper allSquaresFlat
  where
    options :: SudokuWithOptions
    options = genInitialOptions s

    helper :: Pos -> Property
    helper pos
      | s !!! pos > 0   = options !!! pos === One (s !!! pos)
      | otherwise       = conjoin $ map (helper2 pos) $ toList $ options !!! pos

    -- check that no squares of s "similar" to pos contain val
    helper2 :: Pos -> Int -> Property
    helper2 (i,j) val = checkRow .&&. checkCol .&&. checkBox
      where
        checkRow = notElem val $ getRow s i
        checkCol = notElem val $ getCol s j
        checkBox = notElem val $ getBoxFlat s $ getBoxFromCoord (i, j)

testBasic :: IO ()
testBasic = quickCheck $ forAll genSudoku propGenInitialOptions
