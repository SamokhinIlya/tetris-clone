module Game.Input where

import Input
import Data.Map.Strict

data Move = MoveLeft | MoveRight | MoveDown deriving Show

mkMove :: Input -> Maybe Move
mkMove input
  | isPressed $ keyboard input ! KBDown = Just MoveDown
  | otherwise =
    let
      moveLeft  = justPressed $ keyboard input ! KBLeft
      moveRight = justPressed $ keyboard input ! KBRight
    in
      case (moveLeft, moveRight) of
        (True , True ) -> Nothing
        (False, False) -> Nothing
        (True , False) -> Just MoveLeft
        (False, True ) -> Just MoveRight

data Turn = TurnLeft | TurnRight deriving Show

mkTurn :: Input -> Maybe Turn
mkTurn input =
  let
    turnLeft  = justPressed . lmb $ mouse input
    turnRight = justPressed . rmb $ mouse input
  in
    case (turnLeft, turnRight) of
      (True , True ) -> Nothing
      (False, False) -> Nothing
      (True , False) -> Just TurnLeft
      (False, True ) -> Just TurnRight