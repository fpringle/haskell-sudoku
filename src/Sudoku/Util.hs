module Sudoku.Util where

import Data.List

import Sudoku.Defs

-- access
getRow :: Sudoku -> Int -> [Int]
getRow = (!!)

getCol :: Sudoku -> Int -> [Int]
getCol s x = [r !! x | r <- s]

getBox :: Sudoku -> Int -> [[Int]]
getBox s x =
  let
    i = x `div` 3
    j = x `mod` 3
    i1 = i * 3
    i2 = i1 + 2
    j1 = j * 3
    j2 = j1 + 2
  in
    [[s !!! (row, col) | col <- [j1 .. j2]] | row <- [i1 .. i2]]

getBoxFlat :: Sudoku -> Int -> [Int]
getBoxFlat s  = concat . getBox s

-- printing
showSudoku :: Sudoku -> String
showSudoku = intercalate "\n" . map (concat . map (\x -> if x == 0 then " " else show x))

showSudokuNice :: Sudoku -> String
showSudokuNice s =
  let
    horz = "------+-------+------"
    _ls = map (map (\x -> if x == 0 then " " else show x)) s
    ls = [intercalate " " (take 3 r ++ ["|"] ++ take 3 (drop 3 r) ++ ["|"] ++ drop 6 r) | r <- _ls]
  in
    unlines $ (take 3 ls ++ [horz] ++ take 3 (drop 3 ls) ++ [horz] ++ drop 6 ls)

printSudoku :: Sudoku -> IO ()
printSudoku = putStrLn . showSudoku

printSudokuNice :: Sudoku -> IO ()
printSudokuNice = putStrLn . showSudokuNice

-- parsing

-- parse a sudoku grid from a string
parseSudoku :: String -> Sudoku
parseSudoku = map (map (\x -> if x == '.' then 0 else (read [x] :: Int))) . lines

-- read a file and parse the sudoku grid
readFromFile :: FilePath -> IO Sudoku
readFromFile fp = readFile fp >>= return . parseSudoku
