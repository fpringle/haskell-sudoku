{-# LANGUAGE OverloadedStrings #-}
module Sudoku.Server.Server where

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
server :: Application
server request respond = do
  let method = requestMethod request
  case method of
    "GET"  -> handleGet request respond
    "POST" -> handlePost request respond
    other  -> handleUnknownMethod other respond
