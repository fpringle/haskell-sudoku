{-# LANGUAGE OverloadedStrings #-}
module Sudoku.Server.Post where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B

import Data.Aeson
import Network.HTTP.Types.Status (ok200, badRequest400, notFound404)
import Network.Wai

import Sudoku.Defs
import Sudoku.Solve.Backtracking
import Sudoku.Server.Util


handleSolve :: Application
handleSolve request respond = do
  body <- strictRequestBody request
  let decoded = decode body :: Maybe Sudoku
  case decoded of
    Nothing     -> respond (responseLBS badRequest400 [] ("Invalid grid format"))
    Just grid   -> case backtrack grid of
        Nothing     -> respond (responseLBS ok200 [] ("Couldn't solve grid :("))
        Just solved -> do
            let serialized = encode solved
            respond (responseLBS ok200 [] serialized)

handleBadPath :: B.ByteString -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleBadPath path respond = respond $ responseLBS notFound404 [] $ BL.fromStrict ("Invalid request path: " <> path)

handlePost :: Application
handlePost request respond = do
  let path = pathInfo request
  case path of
    ["solve"]   -> handleSolve request respond
    other       -> handleBadPath (rawPathInfo request) respond
