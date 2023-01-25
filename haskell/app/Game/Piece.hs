{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE NumericUnderscores #-}

module Game.Piece
  ( Piece
  , mkPiece
  , PieceType (Square, Stick, L, ReverseL, T, S, ReverseS)
  , dims
  , rowsCols
  , blueprint
  , turn
  ) where

import Data.Array
import Data.Bits ((.&.))
import Data.List
import Data.List.Split
import Data.Word

import Game.Field hiding (dims)
import qualified Game.Field as Field (dims)
import Game.Input

data Piece = Piece { ty :: PieceType, rotations :: Word8 }

data PieceType =
  Square
  | Stick
  | L
  | ReverseL
  | T
  | S
  | ReverseS
  deriving Enum

fromBool :: Bool -> Cell
fromBool True  = Falling
fromBool False = Empty

fromBin :: Word8 -> [Cell]
fromBin bin = 
  [ fromBool $ bin .&. 0b_1000 /= 0
  , fromBool $ bin .&. 0b_0100 /= 0
  , fromBool $ bin .&. 0b_0010 /= 0
  , fromBool $ bin .&. 0b_0001 /= 0
  ]

mkBlueprint :: [Word8] -> Array (Int, Int) Cell
mkBlueprint bs = array ((0, 0), (3, 3)) $ zip [(y, x) | y <- [0..3], x <- [0..3]] $ concatMap fromBin bs

mkPiece :: Piece
mkPiece = Piece { ty = Square, rotations = 0 }

next :: Piece -> Piece
next p = p { ty = succ $ ty p }

-- TODO: mod or %?
turn :: Turn -> Piece -> Piece
turn TurnLeft  p = p { rotations = pred (rotations p) `mod` 4 }
turn TurnRight p = p { rotations = succ (rotations p) `mod` 4 }

dims :: Piece -> (Int, Int)
dims p =
  let
    b      = blueprint p
    (h, w) = Field.dims b
    (rows, cols) = rowsCols b
  in
    ( maybe h ((+1) . fst) . find anyIsFalling . reverse . zip [0..] $ rows
    , maybe w ((+1) . fst) . find anyIsFalling . reverse . zip [0..] $ cols
    )

rowsCols :: Array (Int, Int) Cell -> ([[Cell]], [[Cell]])
rowsCols a =
  ( chunksOf w . map snd .                    assocs $ a
  , chunksOf h . map snd . sortBy colsFirst . assocs $ a
  )
  where
    (h, w) = Field.dims a
    colsFirst ((_, x), _) ((_, x'), _) = compare x x'

anyIsFalling :: (Int, [Cell]) -> Bool
anyIsFalling (_, cs) = any isFalling cs
  where
    isFalling Falling = True
    isFalling _       = False

blueprint :: Piece -> Array (Int, Int) Cell
blueprint p = 
  let
    rotated = iterate (rotate False) blueprint !! (fromInteger . toInteger $ rotations p)
    (rows, cols) = rowsCols rotated
    (srcY, srcX) =
      ( maybe 0 fst . find anyIsFalling . zip [0..] $ rows
      , maybe 0 fst . find anyIsFalling . zip [0..] $ cols
      )
    ((y0, x0), (y1, x1)) = bounds rotated
    e y x a =
      let (y', x') = (y + srcY, x + srcX)
      in if inBounds y' x' a
        then a ! (y', x)
        else Empty
      where
        inBounds y x a =
          let ((y0, x0), (y1, x1)) = bounds a
          in y >= y0 && y <= y1
            && x >= x0 && x <= x1
  in
    array (bounds rotated) [((y, x), e y x rotated) | y <- [y0..y1], x <- [x0..x1]]
  where 
    blueprint = case ty p of
      Square -> mkBlueprint
        [ 0b_1100
        , 0b_1100
        , 0b_0000
        , 0b_0000
        ]
      Stick -> mkBlueprint
        [ 0b_1111
        , 0b_0000
        , 0b_0000
        , 0b_0000
        ]
      L -> mkBlueprint
        [ 0b_1000
        , 0b_1000
        , 0b_1100
        , 0b_0000
        ]
      ReverseL -> mkBlueprint
        [ 0b_0100
        , 0b_0100
        , 0b_1100
        , 0b_0000
        ]
      T -> mkBlueprint
        [ 0b_0100
        , 0b_1110
        , 0b_0000
        , 0b_0000
        ]
      S -> mkBlueprint
        [ 0b_0110
        , 0b_1100
        , 0b_0000
        , 0b_0000
        ]
      ReverseS -> mkBlueprint
        [ 0b_1100
        , 0b_0110
        , 0b_0000
        , 0b_0000
        ]
    