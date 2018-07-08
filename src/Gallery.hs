module Gallery
  ( Gallery(..)
  , readGallery
  , writeGallery
  ) where

import Puzzle
import System.Directory
import System.FilePath
import qualified Data.List as List

data Gallery = Gallery [Puzzle]

readGallery :: FilePath -> IO Gallery
readGallery path = do
  print $ "readGallery: " ++ path
  puzzleNames <- filter (not . List.isPrefixOf ".") <$> listDirectory path
  puzzles <- mapM (readPuzzle . (</>) path) puzzleNames
  return $ Gallery puzzles

writeGallery :: FilePath -> Gallery -> IO ()
writeGallery path (Gallery puzzles) = do
  mapM_ (writePuzzle path) puzzles
