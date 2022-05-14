{- | Sudoku puzzle solving.
-}

module Sudoku.Solve
  (
    -- * Possible entries of a cell
    Options(One, Many)
    -- ** Utility functions for Options
    , mapOptions
    , toList
    , makeOptions
    , lengthOptions
    , deleteOption

    -- * Sudoku grid plus current knowledge
    , SudokuWithOptions
    -- ** Utility functions for SudokuWithOptions
    , setOptions
    , optionsToNormal

    -- * Basic solving techniques
    , genInitialOptions
    , eliminateOptions
    , eliminateOptionsAll
  ) where

import Sudoku.Solve.Basic
import Sudoku.Solve.Backtracking
