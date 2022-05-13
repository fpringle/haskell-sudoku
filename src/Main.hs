module Main where

import System.Environment

import Sudoku.Defs
import Sudoku.Util
import Sudoku.Solve.Backtracking

main :: IO ()
main = do
  args <- getArgs
  let file = head args
  sudoku <- readFromFile file
  putStrLn "Trying to solve:"
  printSudokuNice sudoku
  putStrLn ""
  x <- backtrack sudoku
  case x of Nothing     -> putStrLn "No solution :("
            Just sol    -> putStrLn "Found a solution!\n" >> printSudokuNice sol

  return ()
