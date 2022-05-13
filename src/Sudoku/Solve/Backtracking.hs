module Sudoku.Solve.Backtracking where

import Sudoku.Defs
import Sudoku.Util
import Sudoku.Validity


nextPos :: Pos -> Maybe Pos
nextPos (i, j)
  | i == 8 && j == 8    = Nothing
  | j == 8              = Just (i + 1, 0)
  | otherwise           = Just (i, j + 1)

nextBlank :: Sudoku -> Pos -> Maybe Pos
nextBlank s p = do
  np <- nextPos p
  if s !!! np == 0
  then Just np
  else nextBlank s np

place :: Sudoku -> Pos -> Int -> Sudoku
place = placeInGrid

_backtrack :: Sudoku -> Pos -> Maybe Sudoku
_backtrack state pos =
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
            sub = _backtrack new_state next
          in
            case sub of Nothing  -> go2 (n+1)
                        Just sol -> Just sol

backtrack :: Sudoku -> Maybe Sudoku
backtrack s = _backtrack s (0, -1)
