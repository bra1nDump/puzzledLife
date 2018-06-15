module Main where

import Lib

import System.Random
import Piece
import Puzzle
import Prelude as P
import Graphics.Image
import Graphics.Image.Interface
import Graphics.Image.IO

projectRoot = "/Users/kirill/root/programming/projects/puzzledLife/"
viewer = ExternalViewer "open" [] 0

display :: Piece -> IO ()
display image = do
  displayImageUsing viewer True image

cluster :: IO Piece
cluster = readImageRGB VU $ projectRoot ++ "static/cluster.jpg"

centaurus :: IO Piece
centaurus = readImageRGB VU $ projectRoot ++ "static/centaurus.jpg"

frog :: IO Piece
frog = readImageRGB VU $ projectRoot ++ "static/frog.jpg"

sampleMask = [(10,10,200,100), (12,5,150,200), (40,40,70,70)]

testHIP1 :: IO ()
testHIP1 = do
  cluster <- cluster
  centaurus <- centaurus
  let mergedImage = (cluster + centaurus) / 2
  display mergedImage

testPieceMask1 :: IO ()
testPieceMask1 = do
  cluster <- readImageRGB VU $ projectRoot ++ "static/cluster.jpg"
  let mask = fromList (dims cluster) sampleMask
  display $ applyMask mask cluster

testPieceHash1 :: IO ()
testPieceHash1 = do
  cluster <- cluster
  centaurus <- centaurus
  let clusterID = hash cluster
      centaurusID = hash centaurus
      clusterSubPiece = applyMasks sampleMask cluster
      clusterSubPieceID = hash clusterSubPiece
      clusterSubPieceID' = hashWithMask sampleMask cluster
      -- guard clusterSubPieceID = clusterSubPieceID'
  putStrLn "clusterID centaurusID"
  print clusterID
  print centaurusID
  putStrLn "subPiece (cluster)"
  print clusterSubPieceID
  print clusterSubPieceID'
  display clusterSubPiece

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
  gen <- getStdGen
  let (x,gen') = rand (1,10) gen
      (y,_) = rand (1,10) gen
      (z,_) = rand (1,10) gen'
  print [x,y,z]
  let a = [1,2,5]
      b = [43,5]
  print $ [(x,y) | x <- a, y <- b]
  print $ do x <- a
             y <- b
             [(x,y)]
  print $ a >>= (\x -> b >>= (\y -> [(x,y)]))
