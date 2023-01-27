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
  , next
  ) where

import GHC.Arr
import Data.Bits ((.&.))
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Word

import Game.Field hiding (dims)
import qualified Game.Field as Field (dims)
import Game.Input

data Piece = Piece { ty :: PieceType, rotations :: Int } deriving Show

data PieceType =
  Square
  | Stick
  | L
  | ReverseL
  | T
  | S
  | ReverseS
  deriving (Show, Enum)

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
mkBlueprint bs = listArray ((0, 0), (3, 3)) $ concatMap fromBin bs

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
    ( maybe h (h-) . findIndex (any isFalling) . reverse $ rows
    , maybe w (w-) . findIndex (any isFalling) . reverse $ cols
    )

rowsCols :: Array (Int, Int) Cell -> ([[Cell]], [[Cell]])
rowsCols a =
  ( chunksOf w . map snd .                    assocs $ a
  , chunksOf h . map snd . sortBy colsFirst . assocs $ a
  )
  where
    (h, w) = Field.dims a
    colsFirst ((_, x), _) ((_, x'), _) = compare x x'

isFalling :: Cell -> Bool
isFalling Falling = True
isFalling _       = False

blueprint :: Piece -> Array (Int, Int) Cell
blueprint p =
  let
    rotated = iterate (rotate False) bp !! rotations p
    (srcY, srcX) =
      let (rows, cols) = rowsCols rotated
      in
        ( fromMaybe 0 $ findIndex (any isFalling) rows
        , fromMaybe 0 $ findIndex (any isFalling) cols
        )
    getOrEmpty i a = if inRange (bounds a) i then a ! i else Empty
  in
    array (bounds rotated) [((y, x), getOrEmpty (y + srcY, x + srcX) rotated) | (y, x) <- range (bounds rotated)]
  where 
    bp = case ty p of
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
    