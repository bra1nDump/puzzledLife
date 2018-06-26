module Link
  ( MiningConfig
  , mineLink
  , bestMatch
  ) where

import System.Random
import Data.Bits
import Data.Word
import Data.List as List
import Graphics.Image as Image
import Graphics.Image.Interface as Interface

import Mask
import Piece

data MiningConfig = MiningConfig
  { targetStayAheadBits :: Word64
  , minMaskCount :: Int
  , maxMaskCount :: Int
  }

mineLink :: StdGen -> Piece -> PieceID -> Word64 -> [Rect]
mineLink gen source targetID stayAheadBits = let
  (rows,columns) = Image.dims source
  (maskCount,gen') = random gen
  rectangles = filter (\(x1,y1,x2,y2) -> x1 <= x2 && y1 <= y2)
    . randomRs ((0,0,0,0),(rows-1,columns-1,rows-1,columns-1)) $ gen'
  maskGroups = partition maskCount rectangles
    where partition n arr =
            let (xs,ys) = splitAt n arr
            in xs:partition n ys
  in head $ dropWhile matchRequestNotMet maskGroups
     where matchRequestNotMet maskGroup = let
             constructedMask = mask (Image.dims source) maskGroup
             divergence = xor targetID $ hash 0 $ applyMask constructedMask source
             in stayAheadBits < divergence


bestMatch :: PieceID -> [PieceID] -> (PieceID,Word64)
bestMatch source targets =
  List.minimumBy (\(_,x1) (_,x2) -> compare x1 x2)
  . zip targets
  . List.map (xor source)
  $ targets

