{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}

module Server
  ( serve ) where

import Control.Monad.Catch
import Control.Monad
import System.IO.Error
import System.FilePath
import qualified Data.ByteString as BS
import GHC.Generics

import Data.Aeson
import Control.Monad.Trans.Class
import Happstack.Server.RqData
import Happstack.Server

import Lib
import Mask
import Piece
import Puzzle
import Gallery
import qualified Data.Map as Map

data PieceComponent = Image | Readme

data Frame = Frame
  { x1 :: Int
  , y1 :: Int
  , x2 :: Int
  , y2 :: Int
  } deriving (Generic, Show)

instance FromJSON Frame

newtype Frames = Frames [Frame]
  deriving (Generic, Show)

instance FromJSON Frames
  -- this instance will be derived automatically

galleryDirectory = "data/puzzles.public"

serve :: Gallery -> IO ()
serve gallery = do
  writeGallery galleryDirectory gallery
  simpleHTTP nullConf $ do
    decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)
    router gallery

pieceHandler :: String -> String -> PieceComponent -> ServerPartT IO Response
pieceHandler puzzleId pieceId component = do
  let piecePath = galleryDirectory </> puzzleId </> pieceId
  case component of
    Readme -> do
      readmeContents <- lift $ readFile $ piecePath </> "readme.md"
      ok . toResponse $ readmeContents
    Image ->
      serveFile (guessContentTypeM mimeTypes) $ piecePath </> "image.jpg"

linkHandler :: Gallery -> String -> String -> ServerPartT IO Response
linkHandler (Gallery puzzles) puzzleId pieceId = do
    let puzzle = head . filter ((==) puzzleId . Puzzle.title) $ puzzles
    (Just (Frames frames)) <- lookFrames
    let rects = map
          (\(Frame {x1,y1,x2,y2}) -> (x1, y1, x2, y2) :: Rect)
          frames
    case transition (read pieceId) rects puzzle of
      Just nextPiece -> do
        lift $ print nextPiece
        ok . toResponse $ nextPiece
      Nothing -> do
        badRequest $ toResponse "Your transition failed"

router :: Gallery -> ServerPartT IO Response
router gallery = dir "puzzles" $ path $ \puzzleId -> path $ \pieceId ->
  msum
  [ dir "readme" $ pieceHandler puzzleId pieceId Readme
  , dir "image" $ pieceHandler puzzleId pieceId Image
  , dir "next" $ linkHandler gallery puzzleId pieceId
  ]

-- Helpers
lookFrames :: ServerPartT IO (Maybe Frames)
lookFrames = do
  request <- askRq
  (Just body) <- lift $ takeRequestBody request
  return . decode . unBody $ body
