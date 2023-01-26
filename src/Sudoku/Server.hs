{-
Copyright (c) 2022, Frederick Pringle
All rights reserved.

This source code is licensed under the BSD-style license found in the
LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE OverloadedStrings #-}

{- | This module lets you run the Sudoku solver as an API. The following paths are supported:

__/board__ (GET)

Query parameters:

    - blanks: the number of blank spaces desired. Use this parameter to control the difficulty of the board

Returns:

@
{
  board: [[...]]    # a 9x9 array containing an unsolved Sudoku grid. Blanks are represented by 0.
}
@


__/solve__ (POST)

JSON body:

@
{
  board: [[...]]    # a 9x9 array containing an unsolved Sudoku grid.
}
@

Returns:

@
{
  board: [[...]]    # a 9x9 array containing the solved Sudoku grid.
}
@
-}

module Sudoku.Server
  (
  module Sudoku.Server.Get
  , module Sudoku.Server.Post
  , module Sudoku.Server.Util

  , server
  ) where

import           Sudoku.Server.Get
import           Sudoku.Server.Post
import           Sudoku.Server.Util

import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL

import           Network.HTTP.Types.Status  (methodNotAllowed405)
import           Network.Wai                (Application, Response,
                                             ResponseReceived, requestMethod,
                                             responseLBS)


handleUnknownMethod :: B.ByteString -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleUnknownMethod method respond =
  respond $ responseLBS methodNotAllowed405 [] ("Unsupported HTTP method: " <> BL.fromStrict method)

-- | The main function of the Server module.
server :: Application
server request respond = do
  let method = requestMethod request
  case method of
    "GET"  -> handleGet request respond
    "POST" -> handlePost request respond
    other  -> handleUnknownMethod other respond
