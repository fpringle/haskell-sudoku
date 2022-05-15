module Sudoku.Server.Get where

import qualified Data.ByteString.Lazy.Char8 as BL

import Network.HTTP.Types.Status (status200)
import Network.Wai (Application, responseLBS)

import Sudoku.Generate


handleGet :: Application
handleGet request respond =
  respond (responseLBS status200 [] (BL.pack "Hello World!"))
