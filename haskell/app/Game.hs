module Game where

import Canvas
import Input
import qualified Game.Draw as Draw

import Data.Array

data Data
    = Data
    { field :: Field
    }

mkData :: Data
mkData
    = Data
    { field = fill4BottomRows mkField
    }
    where fill4BottomRows :: Field -> Field
          fill4BottomRows f = f // [((y, x), Falling) | y <- [y0 .. y1]
                                                      , x <- [x0 .. x1] ]
            where ((y0, x0), (y1, x1)) = bounds f

update :: Data -> Canvas -> Input -> Float -> (Data, Canvas)
update d canvas input dt = (d, Draw.cell white (100, 100) 30 canvas)

data Cell = Empty | Falling | Frozen | Disappearing

type Field = Array (Int, Int) Cell

mkField :: Field
mkField = array ((y0, x0), (y1, x1)) [((y, x), Empty) | y <- [y0 .. y1]
                                                      , x <- [x0 .. x1] ]
    where (y0, y1)        = (0 , height - 1)
          (x0, x1)        = (0 , width - 1 )
          (height, width) = (20, 10        )
