module Puzzle
  ( Puzzle(..)
  , makePuzzle
  , mineLink
  ) where

import qualified Data.Map as Map
import Data.Bits
import System.Random
import Data.Word
import Graphics.Image as I
import Debug.Trace

import Piece

instance (Random a, Random b, Random c, Random d) => Random (a,b,c,d) where
  randomR ((a1, a2, a3, a4), (a1', a2', a3', a4')) gen1 =
    let (a1'', gen2) = randomR (a1, a1') gen1
        (a2'', gen3) = randomR (a2, a2') gen2
        (a3'', gen4) = randomR (a3, a3') gen3
        (a4'', gen5) = randomR (a4, a4') gen4
    in ((a1'',a2'',a3'',a4''), gen5)

data Puzzle = Puzzle
  { author :: String
  , pieces :: Map.Map PieceID Piece
  }

makePuzzle :: String -> [Piece] -> Puzzle
makePuzzle author pieces = Puzzle
  { author=author
  , pieces=Map.fromList [(id,piece) | piece <- pieces, let id = hash piece]
  }

mineLink :: Int -> StdGen -> Word64 -> PieceID -> PieceID -> Puzzle -> [Rect]
mineLink maskCount gen percision sourceID targetID puzzle = let
  Just source = Map.lookup sourceID $ pieces puzzle
  (rows,columns)= I.dims source
  randomMasks = filter (\(x1,y1,x2,y2) -> x1 <= x2 && y1 <= y2)
    . randomRs ((0,0,0,0),(rows-1,columns-1,rows-1,columns-1)) $ gen
  setsOfMarks = partition maskCount randomMasks
    where partition n arr =
            let (xs,ys) = splitAt n arr
            in xs:partition n ys
  in head $ dropWhile (\masks -> let
                          subPieceHash = hashWithMask masks source
                          divergence = xor targetID subPieceHash
                          in trace (show divergence) percision < divergence)
     setsOfMarks
