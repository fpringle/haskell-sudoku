module Sudoku.Solve.Backtracking where

import Sudoku.Defs
import Sudoku.Util
import Sudoku.Validity

-- backtracking stuff
{-
type Stack = [(Pos, Int)]

type BacktrackState = State (Sudoku, Stack) ()
-}


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
place s (i, j) n =
  let ith = s !! i
  in take i s ++ [take j ith ++ [n] ++ drop (j+1) ith] ++ drop (i+1) s

_backtrack :: Sudoku -> Pos -> IO (Maybe Sudoku)
_backtrack state pos =
  if not $ isValid state
  then do
    --putStrLn "invalid"
    return Nothing
  else go $ nextBlank state pos
  where
    go :: Maybe Pos -> IO (Maybe Sudoku)
    go Nothing      = return (Just state)
    go (Just next)  =
      do
        --print state
        --printSudokuNice state
        --putStrLn ("Backtracking from " ++ show pos ++ " at position " ++ (show next))
        go2 1
      where
        go2 :: Int -> IO (Maybe Sudoku)
        go2 10 = do
          --putStrLn "FAIL"
          return Nothing
        go2 n =
          do
            -- putStrLn ("  try " ++ (show n))
            let new_state = place state next n
            sub <- _backtrack new_state next
            case sub of Nothing  -> go2 (n+1)
                        Just sol -> return $ Just sol

backtrack :: Sudoku -> IO (Maybe Sudoku)
backtrack s = _backtrack s (0, -1)

