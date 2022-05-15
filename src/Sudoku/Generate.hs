module Sudoku.Generate where

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

generateSolveableFromFirst :: (Monad m, RandomGen g) => Pos -> Int -> Int -> g -> m Sudoku
generateSolveableFromFirst pos val blanks generator =
  clearCells blanks generator $ generateSolvedFromFirst pos val
  where
    clearCells :: (Monad m, RandomGen g) => Int -> g -> Sudoku -> m Sudoku
    clearCells 0 _ cur   = return cur
    clearCells n gen cur =
      let
        notBlank = filter (\pos -> cur !!! pos /= 0) allSquaresFlat
      in
        if notBlank == []
        then return cur
        else do
            next <- clearCell notBlank gen cur
            clearCells (n-1) gen $ next

    clearCell :: (Monad m, RandomGen g) => [Pos] -> g -> Sudoku -> m Sudoku
    clearCell notBlank gen cur = do
      let i = head $ randomRs (0, length notBlank - 1) gen :: Int
      return $ place cur (notBlank !! i) 0

{- | Generate a solveable Sudoku grid with a given number of empty spaces
-}
generateSolveable :: Int -> IO Sudoku
generateSolveable blanks = do
  gen <- newStdGen
  let [i,j,y] = take 3 $ randomRs (0, 8) gen :: [Int]
  generateSolveableFromFirst (i, j) (y + 1) blanks gen
