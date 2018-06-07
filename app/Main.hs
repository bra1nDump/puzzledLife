module Main where

import Lib
import Piece
import Prelude as P
import Graphics.Image
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

testHIP1 :: IO ()
testHIP1 = do
  cluster <- readImageRGB VU $ projectRoot ++ "static/cluster.jpg"
  centaurus <- readImageRGB VU $ projectRoot ++ "static/centaurus.jpg"
  let mergedImage = (cluster + centaurus) / 2
  displayImageUsing viewer True mergedImage

testPieceMask1 :: IO ()
testPieceMask1 = do
  cluster <- readImageRGB VU $ projectRoot ++ "static/cluster.jpg"
  let mask = fromList (dims cluster) [(10,10,200,100), (12,5,150,200), (40,40,70,70)]
  displayImageUsing viewer True $ applyMask mask cluster

main :: IO ()
main = do
  testPieceMask1
