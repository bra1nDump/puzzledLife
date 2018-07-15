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

pieceHandler = uriRest $ \piecePath -> do
  let absolutePath =  galleryDirectory </> makeRelative "/" piecePath
      filename = takeFileName absolutePath
  lift $ print absolutePath
  if filename == "readme.md"
    then do
    readmeContents <- lift $ readFile absolutePath
    ok . toResponse $ readmeContents
    else
    serveFile (guessContentTypeM mimeTypes) $ absolutePath

linkHandler :: Gallery -> ServerPartT IO Response
linkHandler (Gallery puzzles) = do
  path $ \puzzleTitle -> do
    let puzzle = head . filter ((==) puzzleTitle . Puzzle.title) $ puzzles
    path $ \pieceID -> do
      (Just (Frames frames)) <- lookFrames
                              --(\ioError -> do {lift $ print ioError >> return Nothing})
      lift $ print frames
      let rects = map
            (\(Frame {x1,y1,x2,y2}) -> (x1, y1, x2, y2) :: Rect)
            frames
          _ = pieceID :: PieceID
      lift $ print rects
      --method POST
      case transition pieceID rects puzzle of
        Just nextPiece -> do
          lift $ print nextPiece
          ok . toResponse $ nextPiece
        Nothing -> do
          badRequest $ toResponse "Your transition failed"

router :: Gallery -> ServerPartT IO Response
router gallery = msum
  [ dir "public" $ serveDirectory DisableBrowsing ["index.html", "main.js"] "frontend/build"
  , dir "puzzles" pieceHandler
  , dir "link" $ linkHandler gallery
  ]

-- Helpers
lookFrames :: ServerPartT IO (Maybe Frames)
lookFrames = do
  request <- askRq
  (Just body) <- lift $ takeRequestBody request
  lift $ print body
  lift . print . unBody $ body
  return . decode . unBody $ body
