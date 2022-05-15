module Sudoku.Server.Post where

import qualified Data.ByteString.Lazy.Char8 as BL

import Network.HTTP.Types.Status (status200)
import Network.Wai (Application, responseLBS)

handlePost :: Application
handlePost request respond =
  respond (responseLBS status200 [] (BL.pack "Hello World!"))
