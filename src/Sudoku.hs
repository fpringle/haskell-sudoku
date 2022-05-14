{- | A library for generating, checking and solving Sudoku puzzles.
-}

module Sudoku
  (
    -- * Sudoku grid
    Sudoku
    -- ** Grid access functions
    , (!!!)
    , getRow
    , getCol
    , getBox
    , getBoxFlat
    , placeInGrid
    , place
    -- ** Grid utility functions
    -- *** Parsing
    , parseSudoku
    , readFromFile
    -- *** Printing
    , showSudoku
    , showSudokuNice
    , printSudoku
    , printSudokuNice
    -- ** Grid utility variables
    , blank

    -- * Cell position
    , Pos
    -- ** Cell utility functions
    , getBoxCoords
    , getBoxFromCoord
    , similar
    -- ** Cell utility variables
    , allSquares
    , allSquaresFlat
  ) where

import Sudoku.Defs
import Sudoku.Generate
import Sudoku.Util
import Sudoku.Validity
import Sudoku.Solve.Backtracking
import Sudoku.Solve.Basic
