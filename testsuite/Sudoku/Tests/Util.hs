{-
Copyright (c) 2022, Frederick Pringle
All rights reserved.

This source code is licensed under the BSD-style license found in the
LICENSE file in the root directory of this source tree.
-}

module Sudoku.Tests.Util where

import           Test.QuickCheck
import           Test.QuickCheck.Instances.Tuple

import           Sudoku.Tests.TestUtils
import           Sudoku.Types
import           Sudoku.Util


propGetBoxCoords :: Int -> Property
propGetBoxCoords x = conjoin $ map ((===x) . getBoxFromCoord) $ getBoxCoordsFlat x

propShowParse :: Sudoku -> Property
propShowParse s =
  let
    showed = map (\c -> if c == ' ' then '.' else c) $ showSudoku s
  in
    parseSudoku showed === s

propPlace :: Sudoku -> Pos -> Int -> Property
propPlace s pos x =
  let
    placed = place s pos x
  in
    conjoin $ map (\p -> if p == pos then (placed !!! p) === x else placed !!! p === s !!! p) allSquaresFlat


testUtil :: IO ()
testUtil =
  do
    quickCheck $ forAll (elements [0..8]) propGetBoxCoords
    quickCheck $ forAll genSudoku propShowParse
    quickCheck $ forAll ((>**<) genSudoku genPos $ elements [0 .. 9]) $ uncurry3 propPlace
