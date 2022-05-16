{- | A library for generating, checking and solving Sudoku puzzles.
-}

module Sudoku
  (
    -- * Sudoku grid
    Grid
    , Sudoku
    -- ** Grid access functions
    , (!!!)
    , getRow
    , getCol
    , getBox
    , getBoxFlat
    , place
    -- ** Grid utility functions
    , replaceValues
    -- *** Parsing
    , parseSudoku
    , readFromFile
    , readFromCSV
    -- *** Printing
    , showSudoku
    , showSudokuNice
    , printSudoku
    , printSudokuNice
    -- ** Grid utility variables
    , blank
    -- ** Grid generation
    , generateSolved
    , generateSolveable
    , flipRandom

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
-- import Sudoku.Solve.Backtracking
-- import Sudoku.Solve.Basic
