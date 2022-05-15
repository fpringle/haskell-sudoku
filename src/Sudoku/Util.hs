module Sudoku.Util where

import Data.List
import System.IO

import qualified System.IO.Strict as Strict

import Sudoku.Defs

{- | get a row of the grid
-}
getRow :: Sudoku -> Int -> [Int]
getRow (Grid s) = (!!) s

{- | get a column of the grid
-}
getCol :: Sudoku -> Int -> [Int]
getCol (Grid s) x = [r !! x | r <- s]

{- | get the coordinates of a box (ordered right to left, top to bottom) as a Grid
-}
getBoxCoords :: Int -> Grid Pos
getBoxCoords x =
  let
    i = x `div` 3
    j = x `mod` 3
    i1 = i * 3
    i2 = i1 + 2
    j1 = j * 3
    j2 = j1 + 2
  in
    Grid [[(row, col) | col <- [j1 .. j2]] | row <- [i1 .. i2]]

{- | get the coordinates of a box (ordered right to left, top to bottom) as a Grid
-}
getBoxCoordsFlat :: Int -> [Pos]
getBoxCoordsFlat x = let Grid xs = getBoxCoords x in concat xs

{- | get a box of the grid as a Grid
-}
getBox :: Grid a -> Int -> Grid a
getBox s x = fmap (s !!!) $ getBoxCoords x

{- | get a box of the grid as a list
-}
getBoxFlat :: Grid a -> Int -> [a]
getBoxFlat s x = map (s !!!) $ getBoxCoordsFlat x

{- | get the index of the box a cell belongs to
-}
getBoxFromCoord :: Pos -> Int
getBoxFromCoord (i, j) = (div i 3) * 3 + (div j 3)

{- | all cell positions as a 2x2 Grid
-}
allSquares :: Grid Pos
allSquares = Grid [[(i, j) | j <- [0 .. 8]] | i <- [0 .. 8]]

{- | all cell positions as a list
-}
allSquaresFlat :: [Pos]
allSquaresFlat = [(i, j) | i <- [0 .. 8], j <- [0 .. 8]]

-- printing

{- | convert a grid to a simple string representation - blanks are represented as '.'
-}
showSudoku :: Sudoku -> String
showSudoku (Grid s) = intercalate "\n" $ map (concat . map (\x -> if x == 0 then " " else show x)) s

{- | convert a grid to a pretty string representation
-}
showSudokuNice :: Sudoku -> String
showSudokuNice (Grid s) =
  let
    horz = "------+-------+------"
    _ls = map (map (\x -> if x == 0 then " " else show x)) s
    ls = [intercalate " " (take 3 r ++ ["|"] ++ take 3 (drop 3 r) ++ ["|"] ++ drop 6 r) | r <- _ls]
  in
    unlines $ (take 3 ls ++ [horz] ++ take 3 (drop 3 ls) ++ [horz] ++ drop 6 ls)

{- | print a sudoku grid in its simple string representation - see 'showSudoku'
-}
printSudoku :: Sudoku -> IO ()
printSudoku = putStrLn . showSudoku

{- | print a sudoku grid in its pretty string representation - see 'showSudokuNice'
-}
printSudokuNice :: Sudoku -> IO ()
printSudokuNice = putStrLn . showSudokuNice

-- parsing

{- | parse a sudoku grid from a string
-}
parseSudoku :: String -> Sudoku
parseSudoku = Grid . map (map (\x -> if x == '.' then 0 else (read [x] :: Int))) . lines

{- | read a file and parse the sudoku grid
-}
readFromFile :: FilePath -> IO Sudoku
readFromFile fp = readFile fp >>= return . parseSudoku

{- | Parse a line of a CSV file in the format in
 [this kaggle dataset](https://www.kaggle.com/datasets/rohanrao/sudoku#:~:text=single%20unique%20solution.-,Content,-Each%20row%20represents)
-}
parseCSVLine :: String -> (Sudoku, Sudoku)
parseCSVLine s =
  let [puz, sol] = words $ map (\c -> if c == ',' then ' ' else c) s
  in (parseCSVCol puz, parseCSVCol sol)
  where
    parseCSVCol :: String -> Sudoku
    parseCSVCol s = fmap (\(i,j) -> read [s !! (i * 9 + j)]) allSquares

{- | Parse puzzles and solutions a CSV file in the format in
 [this kaggle dataset](https://www.kaggle.com/datasets/rohanrao/sudoku#:~:text=single%20unique%20solution.-,Content,-Each%20row%20represents)
-}
readFromCSV :: FilePath -> IO [(Sudoku, Sudoku)]
readFromCSV fp = Strict.readFile fp >>= return . map parseCSVLine . tail . lines
-- misc

{- | set an entry of a 2x2 grid
-}
placeInGrid :: [[a]] -> Pos -> a -> [[a]]
placeInGrid s (i, j) n =
  let ith = s !! i
  in take i s ++ [take j ith ++ [n] ++ drop (j+1) ith] ++ drop (i+1) s

{- | check if two positions are "similar", i.e. in the same row/box/col but NOT the same
-}
similar :: Pos -> Pos -> Bool
similar (i1, j1) (i2, j2) = (i1, j1) /= (i2, j2) && (
                                i1 == i2 ||
                                j1 == j2 ||
                                getBoxFromCoord (i1,j1) == getBoxFromCoord (i2,j2)
                                )

{- | place a number in a sudoku grid
-}
place :: Sudoku -> Pos -> Int -> Sudoku
place (Grid s) pos = Grid . placeInGrid s pos

