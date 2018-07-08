module Main where

import Lib
import Gallery
import Server (serve)

main :: IO ()
main = do
  gallery <- readGallery "data/puzzles.submissions"
  serve gallery
