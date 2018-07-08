module Piece
  ( Piece(..)
  , PieceID
  , readPiece
  , writePiece
  , display
  , subpiece
  , hash
  , applyMask
  ) where

import Data.Bits
import Graphics.Image.IO
import Graphics.Image as Image
import Graphics.Image.Interface as Interface
import Graphics.Image.ColorSpace as ColorSpace

import Mask
import System.FilePath
import System.Directory
import Debug.Trace

type PieceID = Word64
type PieceImage = Image VS RGB Word8
data Piece = Piece
  { image :: PieceImage
  , readme :: String
  , title :: String
  }

readPiece :: FilePath -> IO Piece
readPiece path = withCurrentDirectory path $ do
  let title = last . splitDirectories $ path
  image <- toWord8I <$> (readImageRGB VS "image.jpg")
  readme <- readFile "readme.md"
  return $ Piece image readme title

writePiece :: FilePath -> Piece -> IO ()
writePiece targetDirectory piece = do
  let pieceID = hash $ image piece
      piecePath = targetDirectory </> show pieceID
  createDirectoryIfMissing True $ piecePath
  withCurrentDirectory piecePath $ do
    writeImage ("image" <.> "jpg") . toDoubleI $ image piece
    writeFile "readme.md" $ readme piece

display :: PieceImage -> IO ()
display = let
  viewer = ExternalViewer "open" [] 0
  in displayImageUsing viewer True

--

subpiece :: [Rect] -> PieceImage -> PieceImage
subpiece maskRanges piece = let
  pieceMask = mask (Image.dims piece) maskRanges
  in applyMask pieceMask piece

applyMask :: Mask -> PieceImage -> PieceImage
applyMask = Image.zipWith
  (\maskBit p -> if isOn maskBit
                 then p
                 else PixelRGB 255 255 255)

hash :: PieceImage -> PieceID
hash image = let
  pieceWord8 = Interface.map ((<$>) toWord8) image
  in Interface.foldl hashPixel 0 pieceWord8
     where hashPixel hash (PixelRGB r g b) =
             (hash + fromIntegral (r + g + b)) `mod` 10000
