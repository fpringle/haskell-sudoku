module Sudoku.Solve.Basic where

import Data.List

import Sudoku.Defs
import Sudoku.Util
import Sudoku.Validity


data Options = One Int | Many [Int]

mapOptions :: (Int -> Int) -> Options -> Options
mapOptions f (One x) = One (f x)
mapOptions f (Many xs) = Many (map f xs)

toList :: Options -> [Int]
toList (One x)   = [x]
toList (Many xs) = xs

makeOptions :: [Int] -> Options
makeOptions [x] = One x
makeOptions xs  = Many xs

lengthOptions :: Options -> Int
lengthOptions (One _)   = 1
lengthOptions (Many xs) = length xs

deleteOption :: Int -> Options -> Options
deleteOption val (One x) = if val == x then Many [] else One x
deleteOption val (Many xs) = makeOptions $ delete val xs

instance Show Options where
  show (One x)   = show x
  show (Many xs) = show xs

instance Eq Options where
  One x   == One y      = x == y
  Many xs == Many ys    = sort xs == sort ys
  One x   == Many xs    = [x] == xs
  Many xs == One x      = [x] == xs

data SudokuWithOptions = SudokuWithOptions [[Options]]
  deriving (Show, Eq)

(!!!!) :: SudokuWithOptions -> Pos -> Options
(SudokuWithOptions s) !!!! (i, j) = s !! i !! j

setOptions :: SudokuWithOptions -> Pos -> [Int] -> SudokuWithOptions
setOptions (SudokuWithOptions opt) pos = SudokuWithOptions . placeInGrid opt pos . makeOptions

genInitialOptions :: Sudoku -> SudokuWithOptions
genInitialOptions s = foldr helper init allSquaresFlat
  where
    init :: SudokuWithOptions
    init = SudokuWithOptions [[Many [] | j <- [0 .. 8]] | i <- [0 .. 8]]

    helper :: Pos -> SudokuWithOptions -> SudokuWithOptions
    helper pos cur
      | s !!! pos > 0   = setOptions cur pos [s !!! pos]
      | otherwise       = setOptions cur pos $ getOptions s pos

    getOptions :: Sudoku -> Pos -> [Int]
    getOptions grid (i, j) = foldr delete [1 .. 9] (row ++ col ++ box)
      where
        row = getRow grid i
        col = getCol grid j
        box = getBoxFlat grid $ getBoxFromCoord (i, j)

optionsToNormal :: SudokuWithOptions -> Sudoku
optionsToNormal (SudokuWithOptions opt) = Sudoku $ map (map helper) opt
  where
    helper :: Options -> Int
    helper (One x)  = x
    helper (Many _) = 0

stepBasic :: SudokuWithOptions -> SudokuWithOptions
stepBasic s = foldr helper s allSquaresFlat
  where
    -- if the square at pos only has one option x, eliminate all occurrences
    -- of x from its row, column and box
    helper :: Pos -> SudokuWithOptions -> SudokuWithOptions
    helper pos grid = case grid !!!! pos of
        Many xs     -> grid
        One x       -> reduceOptions pos x grid

    -- eliminate occurrences
    reduceOptions :: Pos -> Int -> SudokuWithOptions -> SudokuWithOptions
    reduceOptions (i1,j1) x g = SudokuWithOptions $ map (map helper2) allSquares
      where
        helper2 :: Pos -> Options
        helper2 (i2, j2) =
          let ati2j2 = g !!!! (i2, j2)
          in if similar (i1, j1) (i2, j2)
             then deleteOption x ati2j2
             else ati2j2

stepBasicAll :: SudokuWithOptions -> SudokuWithOptions
stepBasicAll opt = let next = stepBasic opt
                   in if next == opt then next else stepBasicAll next
