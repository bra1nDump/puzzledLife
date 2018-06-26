module Puzzle
  ( Puzzle(..)
  , makePuzzle
  , readPuzzle
  , writePuzzle
  ) where

import Control.Monad
import Control.Monad.Trans.Maybe
import qualified Data.List as List
import qualified Data.Map as Map
import Piece
import System.Directory

import Debug.Trace

data Puzzle = Puzzle
  { title :: String
  , pieces :: Map.Map PieceID Piece
  }

makePuzzle :: String -> [Piece] -> Puzzle
makePuzzle title pieces = Puzzle
  { title = title
  , pieces = Map.fromList
             [(id,piece) | piece <- pieces, let id = hash 0 piece]
  }

puzzlesDirectory = "data/puzzles/"

readPuzzle :: String -> IO (Maybe Puzzle)
readPuzzle title = do
  let imagesDirectory = puzzlesDirectory ++ title ++ "/images/"
  puzzleImagesExist <- doesDirectoryExist $ imagesDirectory
  if not puzzleImagesExist
    then return Nothing
    else do
    images <- filter (not . List.isPrefixOf ".") <$> listDirectory imagesDirectory
    print images
    pieces <- sequence . map readPiece . map (imagesDirectory ++) $ images
    return . Just . makePuzzle title $ pieces

writePuzzle :: Puzzle -> IO ()
writePuzzle puzzle = do
  let title' = title puzzle
      pieces' = Map.assocs . Map.mapKeys setPiecePath . pieces $ puzzle
        where setPiecePath hash =
                let path = puzzlesDirectory ++ title' ++ "/" ++ (show hash) ++ ".jpg"
                in trace path path
  forM_ pieces' (\(path,piece) -> writePiece path piece)
