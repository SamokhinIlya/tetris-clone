module Game.Field where

import Data.Array

data Cell = Empty | Falling | Frozen | Disappearing

type Field = Array (Int, Int) Cell

mkField :: Field
mkField = array ((y0, x0), (y1, x1)) [((y, x), Empty) | y <- [y0 .. y1]
                                                      , x <- [x0 .. x1] ]
    where (y0, y1) = (0 , h - 1)
          (x0, x1) = (0 , w - 1)
          (h, w)   = (20, 10   )