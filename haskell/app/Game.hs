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
    where
        fill4BottomRows :: Field -> Field
        fill4BottomRows g = g { gridValues = top ++ map (map (const Falling)) bottom }
            where (top, bottom) = splitAt (gridHeight g - 4) (gridValues g)

update :: Data -> Canvas -> Input -> Float -> (Data, Canvas)
update d canvas input dt = (d, Draw.cell white (100, 100) 30 canvas)

data Grid a
    = Grid
    { gridValues :: [[a]]
    , gridWidth :: Int
    , gridHeight :: Int
    }

mkGrid :: Int -> Int -> a -> Grid a
mkGrid width height value
    = Grid
    { gridValues = values
    , gridWidth = width
    , gridHeight = height
    }
    where values = replicate height row
          row    = replicate width value

data Cell = Empty | Falling | Frozen | Disappearing

type Field = Grid Cell

mkField :: Field
mkField = mkGrid 10 20 Empty