{-
Copyright (c) 2022, Frederick Pringle
All rights reserved.

This source code is licensed under the BSD-style license found in the
LICENSE file in the root directory of this source tree.
-}

-- | A collection of utility functions for working
--   with Sudoku grids.

module Sudoku.Util (
  {- * Printing Sudoku grids
  -}
  showSudoku
  , showSudokuNice
  , printSudoku
  , printSudokuNice

  {- * Parsing Sudoku grids
  -}
  , parseSudoku
  , readFromFile
  -- , parseCSVLine
  , readFromCSV
  ) where

import Data.List
import System.IO

import qualified System.IO.Strict as Strict

import Sudoku.Types

-- | convert a grid to a simple string representation - blanks are represented as '.'
showSudoku :: Sudoku -> String
showSudoku (Grid s) = intercalate "\n" $ map (concatMap (\x -> if x == 0 then " " else show x)) s

-- | convert a grid to a pretty string representation
showSudokuNice :: Sudoku -> String
showSudokuNice (Grid s) =
  let
    horz = "------+-------+------"
    _ls = map (map (\x -> if x == 0 then " " else show x)) s
    ls = [unwords (take 3 r ++ ["|"] ++ take 3 (drop 3 r) ++ ["|"] ++ drop 6 r) | r <- _ls]
  in
    unlines (take 3 ls ++ [horz] ++ take 3 (drop 3 ls) ++ [horz] ++ drop 6 ls)

-- | print a sudoku grid in its simple string representation - see 'showSudoku'
printSudoku :: Sudoku -> IO ()
printSudoku = putStrLn . showSudoku

-- | print a sudoku grid in its pretty string representation - see 'showSudokuNice'
printSudokuNice :: Sudoku -> IO ()
printSudokuNice = putStrLn . showSudokuNice

-- | parse a sudoku grid from a string
parseSudoku :: String -> Sudoku
parseSudoku = Grid . map (map (\x -> if x == '.' then 0 else (read [x] :: Int))) . lines

-- | read a file and parse the sudoku grid
readFromFile :: FilePath -> IO Sudoku
readFromFile fp = parseSudoku <$> readFile fp

-- | Parse a line of a CSV file in the format in
--  [this kaggle dataset](https://www.kaggle.com/datasets/rohanrao/sudoku#:~:text=single%20unique%20solution.-,Content,-Each%20row%20represents)
parseCSVLine :: String -> (Sudoku, Sudoku)
parseCSVLine s =
  let [puz, sol] = words $ map (\c -> if c == ',' then ' ' else c) s
  in (parseCSVCol puz, parseCSVCol sol)
  where
    parseCSVCol :: String -> Sudoku
    parseCSVCol s = fmap (\(i,j) -> read [s !! (i * 9 + j)]) allSquares

-- | Parse puzzles and solutions a CSV file in the format in
--  [this kaggle dataset](https://www.kaggle.com/datasets/rohanrao/sudoku#:~:text=single%20unique%20solution.-,Content,-Each%20row%20represents)
readFromCSV :: FilePath -> IO [(Sudoku, Sudoku)]
readFromCSV fp = map parseCSVLine . tail . lines <$> Strict.readFile fp
