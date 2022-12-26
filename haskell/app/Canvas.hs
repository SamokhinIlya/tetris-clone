module Canvas where

import Data.Word
import Data.Array.Storable

type Canvas = StorableArray (Int, Int) Pixel

type Pixel = Word32

black :: Pixel
black = 0xFF000000

white :: Pixel
white = 0xFFFFFFFF