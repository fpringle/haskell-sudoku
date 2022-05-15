module Main where

import Network.Wai.Handler.Warp (run)

import Sudoku.Server


main :: IO ()
main = run 3421 server
