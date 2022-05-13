module Sudoku.Tests.Defs where

import Test.QuickCheck

import Sudoku.Defs
import Sudoku.Tests.TestUtils

propCorrectHeight :: Sudoku -> Property
propCorrectHeight (Sudoku s) = length s === 9

propCorrectWidths :: Sudoku -> Property
propCorrectWidths (Sudoku s) = conjoin $ map ((===) 9 . length) s

propBlankIsEmpty :: Property
propBlankIsEmpty = let Sudoku s = blank
                   in conjoin $ map (conjoin . map (===0)) s

testDefs :: IO ()
testDefs =
  do
    quickCheck $ forAll genSudoku propCorrectHeight
    quickCheck $ forAll genSudoku propCorrectWidths
    quickCheck $ once propBlankIsEmpty
