module Sudoku.Tests.TestUtils where

import Test.QuickCheck
import Test.QuickCheck.Instances.Tuple

import Sudoku.Defs
import Sudoku.Util
import Sudoku.Validity
import Sudoku.Solve.Basic



genSudoku :: Gen Sudoku
genSudoku = vectorOf 9 $ vectorOf 9 $ elements [0..9]

genSudokuWithOptions :: Gen SudokuWithOptions
genSudokuWithOptions = vectorOf 9 $ vectorOf 9 $ sublistOf [1..9]

genPos :: Gen Pos
genPos = elements [0 .. 8] >*< elements [0 .. 8]

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z