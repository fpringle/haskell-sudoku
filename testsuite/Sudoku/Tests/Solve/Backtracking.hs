{-
Copyright (c) 2022, Frederick Pringle
All rights reserved.

This source code is licensed under the BSD-style license found in the
LICENSE file in the root directory of this source tree.
-}

module Sudoku.Tests.Solve.Backtracking where

import Test.QuickCheck
import Test.QuickCheck.Instances.Tuple

import Sudoku.Types
import Sudoku.Solve.Backtracking

import Sudoku.Tests.TestUtils

propNextPos :: Pos -> Property
propNextPos (8, 8) = nextPos (8, 8) === Nothing
propNextPos (i, j) = do
  let next = nextPos (i, j)
  case next of
    Nothing         -> property False
    Just (ni, nj)   -> ni * 9 + nj === i * 9 + j + 1

propNextBlank :: Sudoku -> Pos -> Property
propNextBlank s pos = do
  let (Grid grid) = s
  let flat = concat grid
  let (i, j) = pos
  let idx = i * 9 + j
  let next = nextBlank s pos
  case next of
    Nothing       -> property $ notElem 0 $ drop (idx + 1) flat
    Just (ni, nj) -> let nidx = ni * 9 + nj
                     in (flat !! nidx == 0) .&&. notElem 0 (drop (idx + 1) $ take nidx flat)
                        

testBacktracking :: IO ()
testBacktracking = do
  quickCheck $ forAll genPos propNextPos
  quickCheck $ forAll (genSudoku >*< genPos) $ uncurry propNextBlank
