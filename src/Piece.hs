module Piece
  ( Piece(..)
  , PieceID
  , applyMask
  , hash
  ) where

import Data.Bits
import Graphics.Image as I
import Graphics.Image.Interface as II
import Graphics.Image.ColorSpace as CS

import Mask

type PieceID = Word64
type Piece = Image VU RGB Double

applyMask :: Mask -> Piece -> Piece
applyMask = I.zipWith (\maskBit p -> if isOn maskBit
                        then p
                        else PixelRGB 255 255 255)

hash :: PieceID -> Piece -> PieceID
hash seed piece = let
  pieceWord8 = II.map ((<$>) toWord8) piece
  in II.foldl hashPixel seed pieceWord8
     where hashPixel hash (PixelRGB r g b) =
             (hash + fromIntegral (r + g + b)) `mod` 1000000)
