module Piece
  ( Piece(..)
  , Mask(..)
--  , hash
  , fromList
  , applyMask
  ) where

import Data.Bits

import Graphics.Image as I
import Graphics.Image.ColorSpace as CS

type Piece = Image VU RGB Double

type Mask = Image VU X Bit

fromList :: (Int,Int) -> [(Int,Int,Int,Int)] -> Mask
fromList dimensions [] =
  makeImageR VU dimensions $ const off
fromList dimensions ((x1,y1,x2,y2):masks) = let
  maskImage = makeImageR VU dimensions
    (\(x,y) -> if x1 <= x && x <= x2 && y1 <= y && y <= y2 then
                on
              else
                off)
  in I.zipWith (\p1 p2 -> if xor (isOn p1) (isOn p2)
                        then on else off)
     maskImage $ fromList dimensions masks

applyMask :: Mask -> Piece -> Piece
applyMask = I.zipWith (\maskPixel p -> if isOn maskPixel then p else PixelRGB 255 255 255)
