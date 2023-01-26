{-
Copyright (c) 2022, Frederick Pringle
All rights reserved.

This source code is licensed under the BSD-style license found in the
LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE OverloadedStrings #-}

-- | Handle POST requests when running the Sudoku solver.

module Sudoku.Server.Post where

import qualified Data.ByteString.Char8     as B
import qualified Data.ByteString.Lazy      as BL

import           Data.Aeson
import           Network.HTTP.Types.Status (badRequest400, notFound404, ok200)
import           Network.Wai

import           Sudoku.Server.Util
import           Sudoku.Solve.Backtracking
import           Sudoku.Types


-- | Handle an HTTP POST reqest on the /solve API path.
handleSolve :: Application
handleSolve request respond = do
  body <- strictRequestBody request
  let decoded = decode body :: Maybe Sudoku
  case decoded of
    Nothing     -> respond (responseLBS badRequest400 [] "Invalid grid format")
    Just grid   -> case backtrack grid of
        Nothing     -> respond (responseLBS ok200 [] "Couldn't solve grid :(")
        Just solved -> do
            let serialized = encode solved
            respond (responseLBS ok200 [] serialized)

-- | Handle an HTTP POST reqest.
handlePost :: Application
handlePost request respond = do
  let path = pathInfo request
  case path of
    ["solve"] -> handleSolve request respond
    other     -> handleBadPath (rawPathInfo request) respond
