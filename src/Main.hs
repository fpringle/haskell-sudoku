{-
Copyright (c) 2022, Frederick Pringle
All rights reserved.

This source code is licensed under the BSD-style license found in the
LICENSE file in the root directory of this source tree.
-}

module Main where

import Network.Wai.Handler.Warp (run)

import Sudoku.Server (server)


main :: IO ()
main = do
  putStrLn "Running Sudoku server on localhost:3421"
  run 3421 server
