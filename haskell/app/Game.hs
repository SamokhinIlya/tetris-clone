module Game where

import Canvas
import Input

import qualified Game.Draw as Draw
import Game.Field

import Data.Array
import Data.Array.IO

data Data
    = Data
    { field     :: Field
    , countdown :: Int
    }

mkData :: Data
mkData
    = Data
    { field = fill4BottomRows mkField
    , countdown = 60
    }
    where
        fill4BottomRows :: Field -> Field
        fill4BottomRows f = f // [((y, x), Falling) | y <- [(y1 - 3) .. y1]
                                                    , x <- [x0 .. x1] ]
            where
                ((_, x0), (y1, x1)) = bounds f

update :: Data -> Canvas -> Input -> Float -> IO (Data, Canvas)
update d canvas input dt = do
    (_, (ch, cw)) <- getBounds canvas
    let (_, (fh, fw)) = bounds . field $ d
        cellPx        = 30
        center        = (ch `div` 2 - (fh * cellPx) `div` 2, cw `div` 2 - (fw * cellPx) `div` 2)
    Draw.field (field d) center cellPx False canvas
    pure (d { countdown = countdown d - 1 }, canvas)
