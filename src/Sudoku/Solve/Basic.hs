{-
Copyright (c) 2022, Frederick Pringle
All rights reserved.

This source code is licensed under the BSD-style license found in the
LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE DeriveFunctor #-}

-- | Basic techniques for solving Sudoku puzzles.

module Sudoku.Solve.Basic (
  -- * Possible entries of a cell
  OneOrMany (..)
  , Options (..)
  -- ** Utility functions for Options
  , countOptions
  , toList
  , makeOptions
  , lengthOptions
  , deleteOption

  -- * Sudoku grid plus current knowledge
  , SudokuWithOptions
  -- ** Utility functions for SudokuWithOptions
  , setOptions
  , optionsToNormal
  , applyUntilStatic

  -- * Basic solving techniques
  , genInitialOptions
  , eliminateOptions
  , eliminateOptionsRepeatedly
  , scanRows
  , scanCols
  , scanBoxes
  , scan
  , scanRepeatedly
  , improve

  ) where

import Data.Function
import Data.List

import Sudoku.Types
import Sudoku.Util
import Sudoku.Validity

-- | A list-like datatype that can contain either one or many elements
data OneOrMany a = One a | Many [a]
  deriving Functor

-- | Options represents our current knowledge about the potential values of
-- a particular cell in a grid.
type Options = OneOrMany Int

instance Foldable OneOrMany where
  foldMap f (One x) = f x
  foldMap f (Many xs) = foldMap f xs

-- | Count the occurrences of an Int in a list of 'Options'
countOptions :: Int -> [Options] -> Int
countOptions x = length . filter (elem x)

-- | Convert an 'Options' object to a list of integers
toList :: Options -> [Int]
toList (One x)   = [x]
toList (Many xs) = xs

-- | Convert a list of integers to an 'Options' object
makeOptions :: [Int] -> Options
makeOptions [x] = One x
makeOptions xs  = Many xs

-- | Get the length of an 'Options' object.
lengthOptions :: Options -> Int
lengthOptions (One _)   = 1
lengthOptions (Many xs) = length xs

-- | Delete an option from an 'Options' object.
deleteOption :: Int -> Options -> Options
deleteOption val (One x) = if val == x then Many [] else One x
deleteOption val (Many xs) = makeOptions $ delete val xs

instance Show a => Show (OneOrMany a) where
  show (One x)   = show x
  show (Many xs) = show xs

-- | Two 'OneOrMany' objects are considered equal if they have the same elements,
-- regardless of constructor or order of elements.
instance Ord a => Eq (OneOrMany a) where
  One x   == One y      = x == y
  Many xs == Many ys    = sort xs == sort ys
  One x   == Many xs    = [x] == xs
  Many xs == One x      = [x] == xs

-- | SudokuWithOptions represents a partially complete sudoku grid,
-- and the knowledge we have about the potential values of each cell
type SudokuWithOptions = Grid Options

-- | Set the options in a cell of the grid
setOptions :: SudokuWithOptions -> Pos -> [Int] -> SudokuWithOptions
setOptions opts pos = place opts pos . makeOptions

-- | Given a function that transforms a SudokuWithOptions and a starting grid,
-- repeatedly apply the function until it converges to a fixed point.
applyUntilStatic :: (SudokuWithOptions -> SudokuWithOptions) -> SudokuWithOptions -> SudokuWithOptions
applyUntilStatic func = fix helper
  where
    helper next guess =
      let nextGuess = func guess
      in if nextGuess == guess then guess else next nextGuess

-- | Convert from 'Sudoku' to 'SudokuWithOptions',
-- eliminating obviously impossible options.
genInitialOptions :: Sudoku -> SudokuWithOptions
genInitialOptions s = foldr helper init allSquaresFlat
  where
    init :: SudokuWithOptions
    init = Grid [[Many [] | j <- [0 .. 8]] | i <- [0 .. 8]]

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

-- | Convert from 'SudokuWithOptions' to 'Sudoku',
-- only taking cells which have only 1 option
optionsToNormal :: SudokuWithOptions -> Sudoku
optionsToNormal = fmap helper
  where
    helper :: Options -> Int
    helper (One x)  = x
    helper (Many _) = 0

-- | eliminate options using a basic method: if a cell at position pos has
-- only one option x, delete all occurrences of x from cells in the same row,
-- column or box as pos
eliminateOptions :: SudokuWithOptions -> SudokuWithOptions
eliminateOptions s = foldr helper s allSquaresFlat
  where
    -- if the square at pos only has one option x, eliminate all occurrences
    -- of x from its row, column and box
    helper :: Pos -> SudokuWithOptions -> SudokuWithOptions
    helper pos grid = case grid !!! pos of
        Many xs     -> grid
        One x       -> reduceOptions pos x grid

    -- eliminate occurrences
    reduceOptions :: Pos -> Int -> SudokuWithOptions -> SudokuWithOptions
    reduceOptions (i1,j1) x g = fmap helper2 allSquares
      where
        helper2 :: Pos -> Options
        helper2 (i2, j2) =
          let ati2j2 = g !!! (i2, j2)
          in if similar (i1, j1) (i2, j2)
             then deleteOption x ati2j2
             else ati2j2

-- | Repeatedly apply 'eliminateOptions' until there are no changes.
eliminateOptionsRepeatedly :: SudokuWithOptions -> SudokuWithOptions
eliminateOptionsRepeatedly = applyUntilStatic eliminateOptions

-- | Given a value x and a list opts, see if there is only one x options in opts.
-- If so, place x at that cell.
scanList :: [Options] -> [Options]
scanList opts = foldr helper opts [1 .. 9]
  where
    helper :: Int -> [Options] -> [Options]
    helper x opts =
      case filter (elem x . (opts !!)) [0 .. 8] of
        [i] -> take i opts ++ [One x] ++ drop (i+1) opts
        _   -> opts

-- | Scan each row to see if there is only 1 place where a value x
-- could be - if there is, place it there
scanRows :: SudokuWithOptions -> SudokuWithOptions
scanRows = mapRows scanList

-- | Scan each column to see if there is only 1 place where a value x
-- could be - if there is, place it there
scanCols :: SudokuWithOptions -> SudokuWithOptions
scanCols = transposeGrid . scanRows . transposeGrid

-- | Scan each box to see if there is only 1 place where a value x
-- could be - if there is, place it there
scanBoxes :: SudokuWithOptions -> SudokuWithOptions
scanBoxes = gridToBoxes . mapRows scanList . gridToBoxes
  where
    gridToBoxes :: Grid a -> Grid a
    gridToBoxes opts = Grid $ map (getBoxFlat opts) [0 .. 8]

-- | Apply 'scanRows', 'scanCols' and 'scanBoxes'
scan :: SudokuWithOptions -> SudokuWithOptions
scan = scanBoxes . scanCols . scanRows

-- | Repeatedly apply 'scan' until convergence.
scanRepeatedly :: SudokuWithOptions -> SudokuWithOptions
scanRepeatedly = applyUntilStatic scan

-- | Improve a Sudoku grid as much as possible using the techniques defined in m'Sudoku.Solve.Basic'.
improve :: Sudoku -> Sudoku
improve = optionsToNormal . scanRepeatedly . genInitialOptions
