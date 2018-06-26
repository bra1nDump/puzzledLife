module Spec where

import Piece

import System.Random
import Piece
import Puzzle
import Prelude as P
import Graphics.Image
import Graphics.Image.Interface
import Graphics.Image.IO

sampleMask = [(10,10,200,100), (12,5,150,200), (40,40,70,70)]



testPuzzleMake1 :: IO ()
testPuzzleMake1 = do
  line <- getLine :: IO String
  cluster <- cluster :: IO Piece
  centaurus <- centaurus
  frog <- frog
  gen <- getStdGen
  let clusterID = hash cluster
      frogID = hash frog
      puzzle = makePuzzle "Bra1nDump" [cluster, centaurus]
      niceMask = mineLink 3 gen 1000 clusterID frogID puzzle
  print niceMask
  display $ applyMasks niceMask cluster
  print $ hashWithMask niceMask cluster
  print $ hashWithMask [(99,5,150,94),(74,126,74,250),(84,168,116,223)] cluster
  print $ hashWithMask [(231,73,260,199),(217,171,255,231),(27,7,265,33)] cluster
  print frogID


rand :: (Int,Int) -> StdGen -> (Int,StdGen)
rand bounds gen = randomR bounds gen

main :: IO ()
main = do
  testPiece
  testPuzzle
