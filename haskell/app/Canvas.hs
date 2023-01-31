module Canvas where

import Data.Array.Storable
import Data.Word

type Canvas = StorableArray (Int, Int) Pixel

type Pixel = Word32

black :: Pixel
black = 0xFF000000

white :: Pixel
white = 0xFFFFFFFF

blue :: Pixel
blue = 0xFFFF0000