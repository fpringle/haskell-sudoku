{-
Copyright (c) 2022, Frederick Pringle
All rights reserved.

This source code is licensed under the BSD-style license found in the
LICENSE file in the root directory of this source tree.
-}

-- | More advanced solution concepts using backtracking techniques.
module Sudoku.Solve.Backtracking (
  backtrack
  , backtrackList
  , nextPos
  , nextBlank
  ) where

import Sudoku.Types
import Sudoku.Util
import Sudoku.Validity
import Sudoku.Solve.Basic

{- | Get the next position in a grid (row-column order), if there is one.                                                               
                                                                                                                                        
@                                                                                                                                       
   nextPos (0, 4) == Just (0, 5)                                                                                                        
   nextPos (2, 8) == Just (3, 0)                                                                                                        
   nextPos (8, 8) == Nothing                                                                                                            
@                                                                                                                                       
-}
nextPos :: Pos -> Maybe Pos
nextPos (i, j)
  | i == 8 && j == 8    = Nothing
  | j == 8              = Just (i + 1, 0)
  | otherwise           = Just (i, j + 1)

-- | Get the next /empty/ position in a grid, if there is one.
nextBlank :: Sudoku -> Pos -> Maybe Pos
nextBlank s p = do
  np <- nextPos p
  if s !!! np == 0
  then Just np
  else nextBlank s np

backtrack' :: Sudoku -> Pos -> Maybe Sudoku
backtrack' = helper . improve
  where
    helper state pos =
      if not $ isValid state
      then Nothing
      else go $ nextBlank state pos
      where
        go :: Maybe Pos -> Maybe Sudoku
        go Nothing      = Just state
        go (Just next)  = go2 1
          where
            go2 :: Int -> Maybe Sudoku
            go2 10 = Nothing
            go2 n =
              let
                new_state = place state next n
                sub = backtrack' new_state next
              in
                case sub of Nothing  -> go2 (n+1)
                            Just sol -> Just sol

-- | Use backtracking to try to find one solution of a suokdu grid.
-- Returns Nothing if there is no solution.
backtrack :: Sudoku -> Maybe Sudoku
backtrack s = backtrack' s (0, -1)

backtrackList' :: Sudoku -> Pos -> [Sudoku]
backtrackList' = helper . improve
  where
    helper state pos =
      if not $ isValid state
      then []
      else go $ nextBlank state pos
      where
        go :: Maybe Pos -> [Sudoku]
        go Nothing      = [state]
        go (Just next)  = go2 1
          where
            go2 :: Int -> [Sudoku]
            go2 10 = []
            go2 n =
              let
                new_state = place state next n
                sub = backtrackList' new_state next
              in
                sub ++ go2 (n+1)

-- | Use backtracking to try to find all solutions of a suokdu grid.
-- Returns Nothing if there is no solution.
backtrackList :: Sudoku -> [Sudoku]
backtrackList s = backtrackList' s (0, -1)
