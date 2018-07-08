{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Mask
  ( Rect
  , Mask
  , mask
  ) where

import System.Random

import Data.Bits
import Graphics.Image as Image
import Graphics.Image.Interface as Interface

type Rect = (Int,Int,Int,Int)
type Mask = Image VS X Bit

to4Tuple :: [a] -> (a,a,a,a)
to4Tuple (x1:x2:x3:x4:_) = (x1,x2,x3,x4)

instance Random Rect where
  random gen = let
    (gen',_) = split gen
    values = randoms gen
    in (to4Tuple values, gen')

  randomR ((f1,f2,f3,f4),(t1,t2,t3,t4)) gen = let
    (x1,gen1) = randomR (f1,t1) gen
    (x2,gen2) = randomR (f2,t2) gen1
    (x3,gen3) = randomR (f3,t3) gen2
    (x4,gen4) = randomR (f4,t4) gen3
    in ((x1,x2,x3,x4),gen3)


mask :: (Int,Int) -> [Rect] -> Mask
mask dimensions [] =
  makeImageR VS dimensions $ const off
mask dimensions ((x1,y1,x2,y2):masks) = let
  maskImage = makeImageR VS dimensions
    (\(x,y) ->
       if x1 <= x && x <= x2
          && y1 <= y && y <= y2
       then on
       else off)
  in Interface.zipWith (\p1 p2 ->
                  if xor (isOn p1) (isOn p2)
                  then on else off)
     maskImage $ mask dimensions masks
