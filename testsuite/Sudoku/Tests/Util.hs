module Sudoku.Tests.Util where

import Test.QuickCheck
import Test.QuickCheck.Instances.Tuple

import Sudoku.Defs
import Sudoku.Util
import Sudoku.Tests.TestUtils


propGetBoxCoords :: Int -> Property
propGetBoxCoords x = conjoin $ map ((===x) . getBoxFromCoord) $ concat $ getBoxCoords x

propShowParse :: Sudoku -> Property
propShowParse s =
  let
    showed = map (\c -> if c == ' ' then '.' else c) $ showSudoku s
  in
    parseSudoku showed === s

propPlace :: (Show a, Eq a) => [[a]] -> Pos -> a -> Property
propPlace s pos x =
  let
    placed = placeInGrid s pos x
  in
    conjoin $ map (\p -> if p == pos then (placed !!! p) === x else placed !!! p === s !!! p) allSquaresFlat


testUtil :: IO ()
testUtil =
  do
    quickCheck $ forAll (elements [0..8]) propGetBoxCoords
    quickCheck $ forAll genSudoku propShowParse
    quickCheck $ forAll ((>**<) genSudoku genPos $ elements [0 .. 9]) $ uncurry3 propPlace
