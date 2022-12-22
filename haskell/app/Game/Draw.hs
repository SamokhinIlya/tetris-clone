module Game.Draw where

import Canvas

import Game.Field

import Data.Array

type Point = (Int, Int)

field :: Field -> Point -> Int -> Bool -> Canvas -> Canvas
field f (x0, y0) cellPx showDisappearing canvas =
    canvas // [((y, x), white) | y <- [y0 .. y1]
                               , x <- [x0 .. x1]
                               , y == y0 || y == y1 || x == x0 || x == x1 ]
        where (y1, x1) = (y0 + fieldHeightPx, x0 + fieldWidthPx)
              (fieldHeightPx, fieldWidthPx) = let (_, (fieldHeight, fieldWidth)) = bounds f
                                              in  (fieldHeight * cellPx, fieldWidth * cellPx)

cell :: Pixel -> Point -> Int -> Canvas -> Canvas
cell color (x, y) cellPx canvas = go color [cellPx, cellPx - 2, cellPx - 8] canvas
    where 
        go color []         canvas = canvas
        go color (px : pxs) canvas = (go (current color) pxs . fillSquare (current color) (x + cellPx - px, y + cellPx - px) (x + px, y + px)) canvas
        background                 = black
        current color              = if color == background then white else black

fillSquare :: Pixel -> Point -> Point -> Canvas -> Canvas
fillSquare color (x0, y0) (x1, y1) canvas =
    canvas // [((y, x), color) | y <- [y0..(y1 - 1)]
                               , x <- [x0..(x1 - 1)]]