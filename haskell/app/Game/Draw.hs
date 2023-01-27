module Game.Draw where

import Canvas

import Game.Field

import Data.Array
import Data.Array.Storable

import Control.Monad

type Point = (Int, Int)

field :: Field -> Point -> Int -> Bool -> Canvas -> IO ()
field f (y0, x0) cellPx showDisappearing canvas = do
  drawBorder canvas
  drawCells canvas
  where
    drawBorder :: Canvas -> IO ()
    drawBorder canvas =
      forM_
        [((y, x), white) | y <- [y0..y1]
                         , x <- [x0..x1]
                         , y `elem` [y0, y1] || x `elem` [x0, x1] ]
        (uncurry $ writeArray canvas)
      where
        (y1  , x1  ) = (y0 + fhPx, x0 + fwPx)
        (fhPx, fwPx) =
          let (_, (fh, fw)) = bounds f
          in
            ((fh + 1) * cellPx, (fw + 1) * cellPx)

    drawCells :: Canvas -> IO ()
    drawCells canvas =
      forM_ (drawable f) $ \((y, x), c) ->
        cell (cellColor c) (y0 + y * cellPx, x0 + x * cellPx) cellPx canvas
      where
        drawable f = filter isDrawable $ assocs f
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
  forM_
    (zip [cellPx, cellPx - 2, cellPx - 8] $ cycle [black, color])
    (\(px, color) -> fillSquare color (x + cellPx - px, y + cellPx - px) (x + px, y + px) canvas)

fillSquare :: Pixel -> Point -> Point -> Canvas -> IO ()
fillSquare color (x0, y0) (x1, y1) canvas =
  forM_
    [((y, x), color) | y <- [y0..(y1 - 1)]
                     , x <- [x0..(x1 - 1)] ]
    (uncurry $ writeArray canvas)

clear :: Canvas -> IO ()
clear canvas = do
  ((y0, x0), (y1, x1)) <- getBounds canvas
  fillSquare black (x0, y0) (x1 + 1, y1 + 1) canvas

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