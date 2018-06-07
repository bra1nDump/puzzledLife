module Puzzle
  ( Puzzle(..)
  ) where

import Piece

data Puzzle = Puzzle
  { author :: String
  , pieces :: [Piece]
  }
