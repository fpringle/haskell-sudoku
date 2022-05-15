module Sudoku.Server.Server where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString as B

import Network.HTTP.Types.Status (status200, methodNotAllowed405)
import Network.Wai
import Network.HTTP.Types.Method

import Sudoku.Server.Get (handleGet)
import Sudoku.Server.Post (handlePost)



handleUnknownMethod :: String -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleUnknownMethod method respond =
  respond $ responseLBS methodNotAllowed405 [] (BL.pack ("Unsupported HTTP method: " ++ method))

server :: Application
server request respond = do
  let method = show $ requestMethod request
  putStrLn ("Method: " ++ method)
  case method of
    "\"GET\""  -> handleGet request respond
    "\"POST\"" -> handlePost request respond
    other  -> handleUnknownMethod other respond
