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
import Game.Field qualified as Field (dims)
import Game.Input

data Piece = Piece
  { ty :: PieceType
  , rotations :: Int
  } deriving Show

data PieceType
  = Square
  | Stick
  | L
  | ReverseL
  | T
  | S
  | ReverseS
  deriving (Show, Enum, Bounded)

mkPiece :: Piece
mkPiece = Piece
  { ty        = Square
  , rotations = 0
  }

next :: Piece -> Piece
next p = p
  { ty        = toEnum (succ (fromEnum (ty p)) `mod` (fromEnum (maxBound :: PieceType) + 1))
  , rotations = 0
  }

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
    ( maybe h (h-) . findIndex (elem Falling) . reverse $ rows
    , maybe w (w-) . findIndex (elem Falling) . reverse $ cols
    )

rowsCols :: Array Pos Cell -> ([[Cell]], [[Cell]])
rowsCols a =
  ( chunksOf w . map snd .               assocs $ a
  , chunksOf h . map snd . sortOn cols . assocs $ a
  )
  where
    (h, w) = Field.dims a
    cols (Pos (_, x), _) = x

blueprint :: Piece -> Array Pos Cell
blueprint p =
  let
    rotated = iterate (rotate False) bp !! rotations p
    (srcY, srcX) =
      let (rows, cols) = rowsCols rotated
      in
        ( fromMaybe 0 $ findIndex (elem Falling) rows
        , fromMaybe 0 $ findIndex (elem Falling) cols
        )
    getOrEmpty i a = if inRange (bounds a) i then a ! i else Empty
  in
    array (bounds rotated)
      [(Pos (y, x), getOrEmpty (Pos (y + srcY, x + srcX)) rotated) | Pos (y, x) <- range (bounds rotated)]
  where 
    bp = mkBlueprint $ case ty p of
      Square   -> [ 0b_1100
                  , 0b_1100
                  , 0b_0000
                  , 0b_0000
                  ]
      Stick    -> [ 0b_1111
                  , 0b_0000
                  , 0b_0000
                  , 0b_0000
                  ]
      L        -> [ 0b_1000
                  , 0b_1000
                  , 0b_1100
                  , 0b_0000
                  ]
      ReverseL -> [ 0b_0100
                  , 0b_0100
                  , 0b_1100
                  , 0b_0000
                  ]
      T        -> [ 0b_0100
                  , 0b_1110
                  , 0b_0000
                  , 0b_0000
                  ]
      S        -> [ 0b_0110
                  , 0b_1100
                  , 0b_0000
                  , 0b_0000
                  ]
      ReverseS -> [ 0b_1100
                  , 0b_0110
                  , 0b_0000
                  , 0b_0000
                  ]

mkBlueprint :: [Word8] -> Array Pos Cell
mkBlueprint bs = listArray (Pos (0, 0), Pos (3, 3)) $ concatMap fromBin bs

fromBin :: Word8 -> [Cell]
fromBin bin = map (fromBool . (/= 0) . (.&. bin)) [ 0b_1000
                                                  , 0b_0100
                                                  , 0b_0010
                                                  , 0b_0001
                                                  ]

fromBool :: Bool -> Cell
fromBool True  = Falling
fromBool False = Empty
    