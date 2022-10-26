{-
Copyright (c) 2022, Frederick Pringle
All rights reserved.

This source code is licensed under the BSD-style license found in the
LICENSE file in the root directory of this source tree.
-}

module Sudoku.Tests.Types where

import Test.QuickCheck

import Sudoku.Types
import Sudoku.Tests.TestUtils

propCorrectHeight :: Sudoku -> Property
propCorrectHeight (Grid s) = length s === 9

propCorrectWidths :: Sudoku -> Property
propCorrectWidths (Grid s) = conjoin $ map ((===) 9 . length) s

propBlankIsEmpty :: Property
propBlankIsEmpty = let Grid s = blank
                   in conjoin $ map (conjoin . map (===0)) s

testDefs :: IO ()
testDefs = do
  quickCheck $ forAll genSudoku propCorrectHeight
  quickCheck $ forAll genSudoku propCorrectWidths
  quickCheck $ once propBlankIsEmpty
