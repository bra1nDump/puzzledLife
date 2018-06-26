module Server
  (server)
  where

import Control.Monad
import Control.Monad.Trans.Class
import Happstack.Server.RqData
import Happstack.Server

import Lib
import Mask
import Piece
import Link
import Puzzle
import qualified Data.Map as Map

server :: IO ()
server = simpleHTTP nullConf router

pieceHandler = uriRest $ \piecePath ->
  serveFile (guessContentTypeM mimeTypes) $ "data/puzzles/" ++ piecePath

linkHandler = path $ \puzzleTitle -> do
  Just puzzle <- lift $ readPuzzle puzzleTitle
  path $ \pieceID -> do
    x1 <- lookRead "x1"
    y1 <- lookRead "y1"
    x2 <- lookRead "x2"
    y2 <- lookRead "y2"
    let rect = (x1, y1, x2, y2) :: Rect
        _ = pieceID :: PieceID
    lift $ print rect
    case Map.lookup pieceID $ pieces puzzle of
      Nothing -> do
        lift $ print $ "this is baad piece not found"
        badRequest $ toResponse "Piece with this ID is not part of the puzzle"
      Just piece -> do
        let pieceID = hash 0 piece
            requestedSubpiece = subpiece [rect] piece
            requestedSubpieceID = hash 0 requestedSubpiece
        lift $ print $ show pieceID
        lift $ print $ "subpieceID" ++ show requestedSubpieceID
        let nextVertice =  show . fst . bestMatch pieceID . filter (/= pieceID) . Map.keys . pieces $ puzzle
        lift $ print nextVertice
        ok . toResponse $ nextVertice

router :: ServerPartT IO Response
router = msum
  [ dir "puzzles" pieceHandler
  , dir "link" linkHandler
  ]
