module Game.Input where

import Input

data Move = MoveLeft | MoveRight | MoveDown deriving Show

mkMove :: Input -> Maybe Move
mkMove input
  | isPressed . down $ keyboard input = Just MoveDown
  | otherwise                         =
    let
      moveLeft  = justPressed . left  $ keyboard input
      moveRight = justPressed . right $ keyboard input
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