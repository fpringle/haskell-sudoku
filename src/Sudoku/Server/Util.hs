{-
Copyright (c) 2022, Frederick Pringle
All rights reserved.

This source code is licensed under the BSD-style license found in the
LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE OverloadedStrings #-}
module Sudoku.Server.Util where

import Data.Aeson

import Sudoku.Defs


instance ToJSON a => ToJSON (Grid a) where
  toJSON (Grid grid) = object ["board" .= grid]

instance FromJSON a => FromJSON (Grid a) where
  parseJSON (Object v) = Grid <$> v .: "board"
