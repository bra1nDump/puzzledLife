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

import Debug.Trace

type PieceID = Word64
type Piece = Image VS RGB Word8

readPiece :: FilePath -> IO Piece
readPiece path = toWord8I <$> (readImageRGB VS path)

writePiece :: FilePath -> Piece -> IO ()
writePiece path piece = let path' = trace path path
  in writeImage path' . toDoubleI $  piece

display :: Piece -> IO ()
display = let
  viewer = ExternalViewer "open" [] 0
  in displayImageUsing viewer True

--

subpiece :: [Rect] -> Piece -> Piece
subpiece maskRanges piece = let
  pieceMask = mask (Image.dims piece) maskRanges
  in applyMask pieceMask piece

applyMask :: Mask -> Piece -> Piece
applyMask = Image.zipWith
  (\maskBit p -> if isOn maskBit
                 then p
                 else PixelRGB 255 255 255)

hash :: PieceID -> Piece -> PieceID
hash seed piece = let
  pieceWord8 = Interface.map ((<$>) toWord8) piece
  in Interface.foldl hashPixel seed pieceWord8
     where hashPixel hash (PixelRGB r g b) =
             (hash + fromIntegral (r + g + b)) `mod` 10000
