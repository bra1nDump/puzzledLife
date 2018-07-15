module Puzzle
  ( Puzzle(..)
  , makePuzzle
  , readPuzzle
  , writePuzzle
  , transition
  ) where

import Control.Monad
import Control.Monad.Trans.Maybe
import qualified Data.List as List
import qualified Data.Map as Map

import Mask
import Link
import Piece
import System.Directory
import System.FilePath

import Debug.Trace

data Puzzle = Puzzle
  { title :: String
  , pieces :: Map.Map PieceID Piece
  }

makePuzzle :: String -> [Piece] -> Puzzle
makePuzzle title' pieces = Puzzle
  { Puzzle.title = title'
  , pieces = Map.fromList
             [(id,piece) | piece <- pieces, let id = hash $ image piece]
  }

readPuzzle :: FilePath -> IO Puzzle
readPuzzle puzzlePath = do
  let puzzleTitle = last . splitDirectories $ puzzlePath
  piecesNames <- filter (not . List.isPrefixOf ".") <$> listDirectory puzzlePath
  print piecesNames
  pieces <- sequence . map (readPiece . (</>) puzzlePath) $ piecesNames
  return $ makePuzzle puzzleTitle pieces

writePuzzle :: FilePath -> Puzzle -> IO ()
writePuzzle puzzlePath puzzle = do
  let title' = Puzzle.title puzzle
      pieces' = pieces puzzle
  forM_ pieces' $ writePiece $ puzzlePath </> title'

transition :: PieceID -> [Rect] -> Puzzle -> Maybe String
transition pieceID frames puzzle = do
  piece <- Map.lookup pieceID $ pieces puzzle
  let pieceImage = image piece
      subpieceID = hash $ subpiece frames pieceImage
  let nextVertice = show . fst
        . bestMatch subpieceID
        . Map.keys . pieces $ puzzle
  return nextVertice
