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

generateSolved :: IO Sudoku
generateSolved = do
  g <- newStdGen
  generateSolvedFromGen g
