module Game.Draw where

import Control.Monad
import Data.Array
import Data.Array.Storable

import Canvas
import Game.Field

type Point = (Int, Int)

field :: Field -> Point -> Int -> Bool -> Canvas -> IO ()
field f (y0, x0) cellPx showDisappearing canvas = do
  drawBorder canvas
  drawCells canvas
  where
    drawBorder :: Canvas -> IO ()
    drawBorder canvas =
      forM_ [((y, x), white) | y <- [y0..y1], x <- [x0..x1], isBorder y x] $
        uncurry $ writeArray canvas
      where
        isBorder y x = y `elem` [y0, y1] || x `elem` [x0, x1]

        (y1, x1) = (y0 + h * cellPx, x0 + w * cellPx)
          where
            (h, w) = dims f

    drawCells :: Canvas -> IO ()
    drawCells canvas =
      forM_ drawableCells $
        \(Pos (y, x), c) -> cell (cellColor c) (y0 + y * cellPx, x0 + x * cellPx) cellPx canvas
      where
        drawableCells = filter isDrawable $ assocs f
          where
            isDrawable (_, c) = case c of
              Falling      -> True
              Frozen       -> True
              Disappearing -> showDisappearing
              Empty        -> False

        cellColor Falling = blue
        cellColor _       = white

cell :: Pixel -> Point -> Int -> Canvas -> IO ()
cell color (y, x) cellPx canvas = do
  forM_ (zip [cellPx, cellPx - 2, cellPx - 8] $ cycle [black, color]) $
    \(px, color) -> fillSquare color (y + cellPx - px, x + cellPx - px) (y + px - 1, x + px - 1) canvas

fillSquare :: Pixel -> Point -> Point -> Canvas -> IO ()
fillSquare color (y0, x0) (y1, x1) canvas =
  forM_ [((y, x), color) | y <- [y0..y1], x <- [x0..x1]] $
    uncurry $ writeArray canvas

clear :: Canvas -> IO ()
clear canvas = do
  (b, e) <- getBounds canvas
  fillSquare black b e canvas

{-
TODO: why this won't work (recursive) but forM_ does?

cell :: Pixel -> Point -> Int -> Canvas -> IO ()
cell color (y, x) cellPx canvas = do
    go color [cellPx] --[cellPx, cellPx - 2, cellPx - 8]
    where 
        go :: Pixel -> [Int] -> IO ()
        go color []         = pure ()
        go color (px : pxs) = do
            print (length pxs)
            let color = current color
            print ((x + cellPx - px, y + cellPx - px), (x + px, y + px))
            putStrLn "begin fill"
            fillSquare color (x + cellPx - px, y + cellPx - px) (x + px, y + px) canvas
            putStrLn "end fill"
            go color pxs
                where
                    current color = if color == background then white else black
                    background    = black

fillSquare :: Pixel -> Point -> Point -> Canvas -> IO ()
fillSquare color (x0, y0) (x1, y1) canvas = do
    putStrLn "begin go"
    go [((y, x), color) | y <- [y0..(y1 - 1)]
                        , x <- [x0..(x1 - 1)] ]
    putStrLn "end go"
        where
            go :: [((Int, Int), Pixel)] -> IO ()
            go []                = pure ()
            go ((i, color) : xs) = do
                print i
                putStrLn "begin writeArray"
                writeArray canvas i color
                putStrLn "end writeArray"
                go xs
-}