module PieceSpec where

import System.Random

import Piece

test :: StdGen -> [Piece] -> IO ()
test gen pieces = do
  let sampleMask = [(10,10,200,100), (12,5,150,200), (40,40,70,70)]
  

testPieceMask :: [Piece] -> IO ()
testPieceMask pieces =
  let mask = fromList (dims cluster) sampleMask
  display $ applyMasks mask cluster

testPieceHash1 :: [Piece] -> []
testPieceHash1 = do
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
