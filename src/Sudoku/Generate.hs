{-
Copyright (c) 2022, Frederick Pringle
All rights reserved.

This source code is licensed under the BSD-style license found in the
LICENSE file in the root directory of this source tree.
-}

module Sudoku.Generate where

import Data.List
import System.Random

import Sudoku.Defs
import Sudoku.Util
import Sudoku.Solve.Backtracking


generateSolvedFromFirst :: Pos -> Int -> Sudoku
generateSolvedFromFirst pos val =
  let x  = backtrack $ place blank pos val
  in case x of Nothing  -> error ("Couldn't solve grid starting at " ++ show pos ++ " = " ++ show val)
               Just sol -> sol


generateSolvedFromGen :: (Monad m, RandomGen g) => g -> m Sudoku
generateSolvedFromGen gen =
  do
    let [i,j,y] = take 3 $ randomRs (0, 8) gen :: [Int]
    return $ generateSolvedFromFirst (i, j) (y+1)

{- | Generate a solved Sudoku grid.
-}
generateSolved :: IO Sudoku
generateSolved = do
  g <- newStdGen
  generateSolvedFromGen g

{- | given a sudoku grid and a generator, replace the numbers
i.e. the same solution but with different numbers (e.g all 1 -> 9, all 5 -> 2 etc)
-}
flipRandom :: (RandomGen g) => g -> Sudoku -> Sudoku
flipRandom generator grid =
  let
    order = 0 : (take 9 $ nub $ randomRs (1, 9) generator :: [Int])
  in
    replaceValues (zip [0 .. 9] order) grid

generateSolveableFromFirst :: (RandomGen g) => Pos -> Int -> Int -> g -> Sudoku
generateSolveableFromFirst pos val blanks generator =
  flipRandom generator $ clearCells blanks generator $ generateSolvedFromFirst pos val
  where
    clearCells :: (RandomGen g) => Int -> g -> Sudoku -> Sudoku
    clearCells n gen cur =
      let
        notBlank = filter (\pos -> cur !!! pos /= 0) allSquaresFlat
        numToDelete = min n $ length notBlank
        toDelete = take numToDelete $ nub $ randomRs (0, length notBlank - 1) gen :: [Int]
      in
        foldr (\i b -> place b (notBlank !! i) 0) cur toDelete

{- | Generate a solveable Sudoku grid with a given number of empty spaces
-}
generateSolveable :: Int -> IO Sudoku
generateSolveable blanks = do
  gen <- newStdGen
  let [i,j,y] = take 3 $ randomRs (0, 8) gen :: [Int]
  return $ generateSolveableFromFirst (i, j) (y + 1) blanks gen
