module Sudoku.Tests.Defs where

import Test.QuickCheck

import Sudoku.Defs
import Sudoku.Tests.TestUtils

propCorrectHeight :: Sudoku -> Property
propCorrectHeight s = length s === 9

propCorrectWidths :: Sudoku -> Property
propCorrectWidths = conjoin . map ((===) 9 . length)

propBlankIsEmpty :: Property
propBlankIsEmpty = conjoin $ map (conjoin . map (===0)) blank

testDefs :: IO ()
testDefs =
  do
    quickCheck $ forAll genSudoku propCorrectHeight
    quickCheck $ forAll genSudoku propCorrectWidths
    quickCheck $ once propBlankIsEmpty
