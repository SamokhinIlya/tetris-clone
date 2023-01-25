module Game.Field where

import Data.Array

data Cell = Empty | Falling | Frozen | Disappearing

type Field = Array (Int, Int) Cell

mkField :: Field
mkField =
  array
    ((y0, x0), (y1, x1))
    [((y, x), Empty) | y <- [y0..y1]
                     , x <- [x0..x1] ]
  where
    (y0, y1) = (0 , h - 1)
    (x0, x1) = (0 , w - 1)
    (h, w)   = (20, 10   )

dims :: Array (Int, Int) Cell -> (Int, Int)
dims a =
  let ((y0, x0), (y1, x1)) = bounds a
  in (y1 - y0 + 1, x1 - x0 + 1)

copyIf :: (Cell -> Bool) -> Field -> (Int, Int) -> (Int, Int) -> Field -> Field
copyIf pred grid (y0, x0) (y1, x1) self =
  let
    (h, w) = dims self
    y1 = min (y0 + y1 - 1) (h - 1)
    x1 = min (x0 + x1 - 1) (w - 1)
  in
    self // [((y, x), if pred (grid ! (y, x)) then grid ! (y, x) else self ! (y, x))
      | y <- [y0..y1], x <- [x0..x1]]

rotate :: Bool -> Field -> Field
rotate left f
  | y0 /= x0 || y1 /= x1 = error "Field must be square"
  | otherwise =
    if left
    then array (bounds f) [((y        , w - 1 - x), f ! (y, x)) | y <- [y0..y1], x <- [x0..x1]]
    else array (bounds f) [((h - 1 + y, x        ), f ! (y, x)) | y <- [y0..y1], x <- [x0..x1]]
  where
    ((y0, x0), (y1, x1)) = bounds f
    (h, w) = dims f