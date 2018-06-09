module Main where

import System.Random

import Lib
import Piece
import Puzzle
import Prelude as P
import Graphics.Image
import Graphics.Image.Interface
import Graphics.Image.IO

-- displayImageUsing
-- :: (Array VS cs e, Array arr cs e, Writable (Image VS cs e) TIF)
-- => ExternalViewer
-- External viewer to use
-- -> Bool
-- Should the call be blocking
-- -> Image arr cs e
-- Image to display
-- -> IO ()

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
  cluster <- cluster
  centaurus <- centaurus
  frog <- frog
  gen <- getStdGen
  let clusterID = hash cluster
      frogID = hash frog
      puzzle = makePuzzle "Bra1nDump" [cluster, centaurus]
      niceMask = mineLink 3 gen 1000 clusterID frogID puzzle
  print niceMask

main :: IO ()
main = do
  testPuzzleMake1
