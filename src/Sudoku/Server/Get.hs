{-# LANGUAGE OverloadedStrings #-}
module Sudoku.Server.Get where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B
import Text.Read

import Data.Aeson
import Network.HTTP.Types.Status (ok200, badRequest400, notFound404)
import Network.Wai

import Sudoku.Defs
import Sudoku.Solve.Backtracking
import Sudoku.Server.Util
import Sudoku.Generate


handleBoard :: Application
handleBoard request respond = do
  let query = queryString request
  case lookup "blanks" query of
    Nothing       -> respond (responseLBS badRequest400 [] ("Number of blanks not specified"))
    Just Nothing  -> respond (responseLBS badRequest400 [] ("\"blanks\" parameter must be an integer"))
    Just (Just b) -> case readMaybe (B.unpack b) :: Maybe Int of
      Nothing -> respond (responseLBS badRequest400 [] ("\"blanks\" parameter must be an integer"))
      Just blanks ->
        if blanks < 0 || blanks > 81
        then respond (responseLBS badRequest400 [] ("\"blanks\" parameter must be an integer between 0 and 81"))
        else do
          grid <- generateSolveable blanks
          let serialized = encode grid
          respond (responseLBS ok200 [] serialized)

handleBadPath :: B.ByteString -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleBadPath path respond = respond $ responseLBS notFound404 [] $ BL.fromStrict ("Invalid request path: " <> path)

handleGet :: Application
handleGet request respond = do
  let path = pathInfo request
  case path of
    ["board"]   -> handleBoard request respond
    other       -> handleBadPath (rawPathInfo request) respond
