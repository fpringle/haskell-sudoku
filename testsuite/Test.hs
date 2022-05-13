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
