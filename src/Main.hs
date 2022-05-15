module Main where

import Network.Wai.Handler.Warp (run)

import Sudoku.Server


main :: IO ()
main = do
  putStrLn "Running Sudoku server on localhost:3421"
  run 3421 server
