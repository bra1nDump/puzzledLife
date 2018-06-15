module Mask
  ( Rect
  , Mask
  , mask)

type Mask = Image VU X Bit
type Rect = (Int,Int,Int,Int)



mask :: (Int,Int) -> [Rect] -> Mask
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
