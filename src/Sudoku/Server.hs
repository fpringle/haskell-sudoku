{-
Copyright (c) 2022, Frederick Pringle
All rights reserved.

This source code is licensed under the BSD-style license found in the
LICENSE file in the root directory of this source tree.
-}

{- | This module lets you run the Sudoku solver as an API.

__Paths__

/board

Methods:

    - GET


Query parameters:

    - blanks: the number of blank spaces desired. Use this parameter to control the difficulty of the board


/solve:

    - Post

JSON body:

    - board: the grid to be solved as a 2-dimension 9x9 array. Blanks are represented by 0.
-}

module Sudoku.Server
  (
  module Sudoku.Server.Server
  , module Sudoku.Server.Get
  , module Sudoku.Server.Post
  , module Sudoku.Server.Util
  ) where

import Sudoku.Server.Server
import Sudoku.Server.Get
import Sudoku.Server.Post
import Sudoku.Server.Util
