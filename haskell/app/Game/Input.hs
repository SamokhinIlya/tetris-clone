module Game.Input where

import Input
import Data.Map.Strict

data Move
  = MoveLeft
  | MoveRight
  | MoveDown
  deriving Show

mkMove :: Input -> Maybe Move
mkMove input
  | isPressed $ keyboard input ! KBDown = Just MoveDown
  | otherwise =
    let
      moveLeft  = justPressed $ keyboard input ! KBLeft
      moveRight = justPressed $ keyboard input ! KBRight
    in
      case (moveLeft, moveRight) of
        (True , False) -> Just MoveLeft
        (False, True ) -> Just MoveRight
        _              -> Nothing

data Turn
  = TurnLeft
  | TurnRight
  deriving Show

mkTurn :: Input -> Maybe Turn
mkTurn input =
  let
    turnLeft  = justPressed $ mouse input ! MouseLeft
    turnRight = justPressed $ mouse input ! MouseRight
  in
    case (turnLeft, turnRight) of
      (True , False) -> Just TurnLeft
      (False, True ) -> Just TurnRight
      _              -> Nothing