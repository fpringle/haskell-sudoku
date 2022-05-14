module Sudoku.Defs where

import Data.List

data Grid a = Grid [[a]]
  deriving (Show, Eq)

instance Functor Grid where
  fmap f (Grid xs) = Grid $ fmap (fmap f) xs

-- type definition
type Sudoku = Grid Int

type Pos = (Int, Int)

(!!!) :: Sudoku -> Pos -> Int
(Grid s) !!! (i, j) = s !! i !! j

infixl 9 !!!


blank :: Sudoku
blank = Grid $ replicate 9 $ replicate 9 0
