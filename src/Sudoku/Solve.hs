{- | Sudoku puzzle solving.
-}

module Sudoku.Solve
  (
    -- * Possible entries of a cell
    Options(One, Many)
    -- ** Utility functions for Options
    , mapOptions
    , elemOptions
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

    -- * More advanced solving techniques
    , backtrack
    , backtrackList
  ) where

import Sudoku.Solve.Basic
import Sudoku.Solve.Backtracking
