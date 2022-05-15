{-# LANGUAGE OverloadedStrings #-}
module Sudoku.Server.Util where

import Data.Aeson

import Sudoku.Defs


instance ToJSON a => ToJSON (Grid a) where
  toJSON (Grid grid) = object ["board" .= grid]

instance FromJSON a => FromJSON (Grid a) where
  parseJSON (Object v) = Grid <$> v .: "board"
