{-
Copyright (c) 2022, Frederick Pringle
All rights reserved.

This source code is licensed under the BSD-style license found in the
LICENSE file in the root directory of this source tree.
-}

-- | Sudoku puzzle solving.

module Sudoku.Solve
  (
    -- * Possible entries of a cell
    Options (..)
    -- ** Utility functions for Options
    , countOptions
    , toList
    , makeOptions
    , lengthOptions
    , deleteOption

    -- * Sudoku grid plus current knowledge
    , SudokuWithOptions
    --
    -- ** Utility functions for SudokuWithOptions
    , setOptions
    , optionsToNormal
    , applyUntilStatic

    -- * Basic solving techniques
    , improve

    -- * More advanced solving techniques
    , backtrack
    , backtrackList
  ) where

import           Sudoku.Solve.Backtracking
import           Sudoku.Solve.Basic
