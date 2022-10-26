{-
Copyright (c) 2022, Frederick Pringle
All rights reserved.

This source code is licensed under the BSD-style license found in the
LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE OverloadedStrings #-}
module Sudoku.Server.Server (
  server
  ) where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString as B

import Network.HTTP.Types.Status (methodNotAllowed405)
import Network.Wai (Application, responseLBS, requestMethod, Response, ResponseReceived)
import Network.HTTP.Types.Method (Method)

import Sudoku.Server.Get (handleGet)
import Sudoku.Server.Post (handlePost)


handleUnknownMethod :: B.ByteString -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleUnknownMethod method respond =
  respond $ responseLBS methodNotAllowed405 [] ("Unsupported HTTP method: " <> BL.fromStrict method)

{- | The main function of the Server module.
-}
server :: Application
server request respond = do
  let method = requestMethod request
  case method of
    "GET"  -> handleGet request respond
    "POST" -> handlePost request respond
    other  -> handleUnknownMethod other respond
