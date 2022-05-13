module Sudoku.Tests.Solve.Backtracking where

import Test.QuickCheck
import Test.QuickCheck.Instances.Tuple

import Sudoku.Defs
import Sudoku.Solve.Backtracking

import Sudoku.Tests.TestUtils

propNextPos :: Pos -> Property
propNextPos (8, 8) = nextPos (8, 8) === Nothing
propNextPos (i, j) = do
  let next = nextPos (i, j)
  case next of
    Nothing         -> property False
    Just (ni, nj)   -> ni * 9 + nj === i * 9 + j + 1

propNextBlank :: Sudoku -> Pos -> Property
propNextBlank s pos = do
  let flat = concat s
  let (i, j) = pos
  let idx = i * 9 + j
  let next = nextBlank s pos
  case next of
    Nothing       -> property $ not $ elem 0 $ drop (idx + 1) flat
    Just (ni, nj) -> let nidx = ni * 9 + nj
                     in (
                        (flat !! nidx == 0) .&&.
                        (not $ elem 0 $ drop (idx + 1) $ take (nidx) flat)
                        )

testBacktracking :: IO ()
testBacktracking = do
  quickCheck $ forAll genPos propNextPos
  quickCheck $ forAll (genSudoku >*< genPos) $ uncurry propNextBlank
