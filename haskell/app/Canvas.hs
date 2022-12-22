module Canvas where

import Data.Word
import Data.Array

type Canvas = Array (Int, Int) Pixel

type Pixel = (Word8, Word8, Word8, Word8)

black :: Pixel
black = (0xFF, 0x00, 0x00, 0x00)

white :: Pixel
white = (0xFF, 0xFF, 0xFF, 0xFF)