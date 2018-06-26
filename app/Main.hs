module Main where

import Lib
import Puzzle
import Server (server)

main :: IO ()
main = do
  braindump <- readPuzzle "braindump"
  case braindump of
    Nothing -> error "sheeet"
    Just puzzle -> writePuzzle puzzle
  server
