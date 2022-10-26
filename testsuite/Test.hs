{-
Copyright (c) 2022, Frederick Pringle
All rights reserved.

This source code is licensed under the BSD-style license found in the
LICENSE file in the root directory of this source tree.
-}

module Main where

import Test.QuickCheck

import Sudoku.Tests.Defs
import Sudoku.Tests.Generate
import Sudoku.Tests.Util
import Sudoku.Tests.Solve.Backtracking
import Sudoku.Tests.Solve.Basic

main :: IO ()
main = do
  testDefs
  testUtil
  testGenerate
  testBacktracking
  testBasic
