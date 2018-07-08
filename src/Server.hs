module Server
  ( serve ) where

import Control.Monad
import Control.Monad.Trans.Class
import Happstack.Server.RqData
import Happstack.Server
import System.FilePath

import Lib
import Mask
import Piece
import Puzzle
import Gallery
import qualified Data.Map as Map

galleryDirectory = "data/puzzles.public"

serve :: Gallery -> IO ()
serve gallery = do
  writeGallery galleryDirectory gallery
  simpleHTTP nullConf $ router gallery

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
linkHandler (Gallery puzzles) = path $ \puzzleTitle -> do
  let puzzle = head . filter ((==) puzzleTitle. Puzzle.title) $ puzzles
  path $ \pieceID -> do
    [x1, y1, x2, y2] <- mapM lookRead ["x1", "y1", "x2", "y2"]
    let rect = (x1, y1, x2, y2) :: Rect
        _ = pieceID :: PieceID
    lift $ print rect
    case transition pieceID [rect] puzzle of
      Just nextPiece -> do
        lift $ print nextPiece
        ok . toResponse $ nextPiece
      Nothing -> do
        badRequest $ toResponse "Your transition failed"

router :: Gallery -> ServerPartT IO Response
router gallery = msum
  [ dir "puzzles" pieceHandler
  , dir "link" $ linkHandler gallery
  ]
