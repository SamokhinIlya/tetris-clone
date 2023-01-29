{-# LANGUAGE FlexibleInstances #-}

module Game.Field where

import GHC.Ix
import GHC.Arr

import Debug.Trace

data Cell = Empty | Falling | Frozen | Disappearing deriving Show

-- newtype needed to show array index error with values
-- default instance for pairs doesn't do that
newtype Pos = Pos (Int, Int) deriving (Eq, Ord, Show)

type Field = Array Pos Cell

instance Ix Pos where
  range (Pos b, Pos e) = map Pos $ range (b, e)

  index (Pos b, Pos e) (Pos i)
    | inRange (b, e) i = unsafeIndex (b, e) i
    | otherwise        = GHC.Ix.indexError (b, e) i "Pos"

  inRange (Pos b, Pos e) (Pos i) = inRange (b, e) i

mkField :: Field
mkField =
  array
    (Pos (y0, x0), Pos (y1, x1))
    [(Pos (y, x), Empty) | y <- [y0..y1]
                         , x <- [x0..x1] ]
  where
    (y0, y1) = (0 , h - 1)
    (x0, x1) = (0 , w - 1)
    (h, w)   = (20, 10   )

dims :: Array Pos Cell -> (Int, Int)
dims a =
  let (Pos (y0, x0), Pos (y1, x1)) = bounds a
  in (y1 - y0 + 1, x1 - x0 + 1)

copyIf :: (Cell -> Bool) -> Field -> (Int, Int) -> (Int, Int) -> Field -> Field
copyIf pred grid (y0, x0) (y1', x1') self =
  let
    (h, w) = dims self
    y1 = min (y0 + y1' - 1) (h - 1)
    x1 = min (x0 + x1' - 1) (w - 1)
  in
    self // [(Pos (y', x'), grid ! Pos (y, x)) | (y, y') <- zip [0..] [y0..y1]
                                               , (x, x') <- zip [0..] [x0..x1]
                                               , pred $ grid ! Pos (y, x)     ]

rotate :: Bool -> Field -> Field
rotate left f
  | y0 /= x0 || y1 /= x1 = error "Field must be square"
  | otherwise =
    if left
    then array bnds [(Pos (w - 1 - x, y        ), f ! Pos (y, x)) | Pos (y, x) <- range bnds]
    else array bnds [(Pos (x        , h - 1 - y), f ! Pos (y, x)) | Pos (y, x) <- range bnds]
  where
    bnds@(Pos (y0, x0), Pos (y1, x1)) = bounds f
    (h, w) = dims f