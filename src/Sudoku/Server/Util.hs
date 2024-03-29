{-
Copyright (c) 2022, Frederick Pringle
All rights reserved.

This source code is licensed under the BSD-style license found in the
LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE OverloadedStrings #-}

-- | Utility functions used by the Sudoku server.

module Sudoku.Server.Util (
  handleBadPath
  , toJSON
  , fromJSON
  ) where

import           Data.Aeson
import qualified Data.ByteString.Char8     as B
import qualified Data.ByteString.Lazy      as BL
import           Network.HTTP.Types.Status (notFound404)
import           Network.Wai

import           Sudoku.Types


instance ToJSON a => ToJSON (Grid a) where
  toJSON (Grid grid) = object ["board" .= grid]

instance FromJSON a => FromJSON (Grid a) where
  parseJSON (Object v) = Grid <$> v .: "board"
  parseJSON other      = error ("invalid grid JSON: " ++ show other)

-- | Handle an invalid path
handleBadPath :: B.ByteString -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleBadPath path respond = respond $ responseLBS notFound404 [] $ BL.fromStrict ("Invalid request path: " <> path)
