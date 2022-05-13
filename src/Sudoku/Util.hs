module Sudoku.Util where

import Data.List

import Sudoku.Defs

-- access
getRow :: Sudoku -> Int -> [Int]
getRow (Sudoku s) = (!!) s

getCol :: Sudoku -> Int -> [Int]
getCol (Sudoku s) x = [r !! x | r <- s]

getBoxCoords :: Int -> [[Pos]]
getBoxCoords x =
  let
    i = x `div` 3
    j = x `mod` 3
    i1 = i * 3
    i2 = i1 + 2
    j1 = j * 3
    j2 = j1 + 2
  in
    [[(row, col) | col <- [j1 .. j2]] | row <- [i1 .. i2]]


getBox :: Sudoku -> Int -> [[Int]]
getBox s x = map (map (s !!!)) $ getBoxCoords x

getBoxFlat :: Sudoku -> Int -> [Int]
getBoxFlat s  = concat . getBox s

getBoxFromCoord :: Pos -> Int
getBoxFromCoord (i, j) = (div i 3) * 3 + (div j 3)

allSquares :: [[Pos]]
allSquares = [[(i, j) | j <- [0 .. 8]] | i <- [0 .. 8]]

allSquaresFlat :: [Pos]
allSquaresFlat = [(i, j) | i <- [0 .. 8], j <- [0 .. 8]]

-- printing

showSudoku :: Sudoku -> String
showSudoku (Sudoku s) = intercalate "\n" $ map (concat . map (\x -> if x == 0 then " " else show x)) s

showSudokuNice :: Sudoku -> String
showSudokuNice (Sudoku s) =
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
parseSudoku = Sudoku . map (map (\x -> if x == '.' then 0 else (read [x] :: Int))) . lines

-- read a file and parse the sudoku grid
readFromFile :: FilePath -> IO Sudoku
readFromFile fp = readFile fp >>= return . parseSudoku


-- misc

-- set an entry of a 2x2 grid
placeInGrid :: [[a]] -> Pos -> a -> [[a]]
placeInGrid s (i, j) n =
  let ith = s !! i
  in take i s ++ [take j ith ++ [n] ++ drop (j+1) ith] ++ drop (i+1) s

-- check if p1 and p2 are "similar", i.e. in the same row/box/col but NOT the same
similar :: Pos -> Pos -> Bool
similar (i1, j1) (i2, j2) = (i1, j1) /= (i2, j2) && (
                                i1 == i2 ||
                                j1 == j2 ||
                                getBoxFromCoord (i1,j1) == getBoxFromCoord (i2,j2)
                                )

place :: Sudoku -> Pos -> Int -> Sudoku
place (Sudoku s) pos = Sudoku . placeInGrid s pos

