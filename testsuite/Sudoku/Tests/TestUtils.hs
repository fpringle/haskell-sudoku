{-
Copyright (c) 2022, Frederick Pringle
All rights reserved.

This source code is licensed under the BSD-style license found in the
LICENSE file in the root directory of this source tree.
-}

module Sudoku.Tests.TestUtils where

import Test.QuickCheck
import Test.QuickCheck.Instances.Tuple

import Sudoku.Types
import Sudoku.Util
import Sudoku.Validity
import Sudoku.Solve.Basic


genSudoku :: Gen Sudoku
genSudoku = Grid <$> (vectorOf 9 $ vectorOf 9 $ elements [0..9])

genPos :: Gen Pos
genPos = elements [0 .. 8] >*< elements [0 .. 8]

instance Arbitrary Options where
  arbitrary = makeOptions <$> sublistOf [1..9]

genSudokuWithOptions :: Gen SudokuWithOptions
genSudokuWithOptions = Grid <$> (vectorOf 9 $ vectorOf 9 $ arbitrary)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z


loadPuzzleAndSolution :: Int -> IO (Sudoku, Sudoku)
loadPuzzleAndSolution num = do
  let puzzlePath = (show num) ++ ".puzzle"
  let solutionPath = (show num) ++ ".solution"
  puzzle <- readFromFile puzzlePath
  solution <- readFromFile solutionPath
  return (puzzle, solution)
