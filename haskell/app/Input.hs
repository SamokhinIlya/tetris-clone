module Input(
  Input, mkInput, mouse, keyboard,
  Mouse, lmb, rmb,
  Keyboard, left, right, down,
  Button, update, isPressed, justPressed)
    where

data Input = Input
  { mouse    :: Mouse
  , keyboard :: Keyboard
  } deriving (Show)

mkInput :: Input
mkInput = Input
  { mouse = mkMouse
  , keyboard = mkKeyboard
  }

data Mouse = Mouse
  { lmb :: Button
  , rmb :: Button
  } deriving (Show)

mkMouse :: Mouse
mkMouse = Mouse
  { lmb = mkButton
  , rmb = mkButton
  }

data Keyboard = Keyboard
  { left  :: Button
  , right :: Button
  , down  :: Button
  } deriving (Show)

mkKeyboard :: Keyboard
mkKeyboard = Keyboard
  { left  = mkButton
  , right = mkButton
  , down  = mkButton
  }

data Button = Button
  { prev :: Bool
  , curr :: Bool
  }

instance Show Button where
  show b =
    (if curr b then "_" else "T") <> (if prev b then "_" else "T")

mkButton :: Button
mkButton = Button
  { prev = False
  , curr = False
  }

update :: Bool -> Button -> Button
update newCurr old = Button
  { prev = curr old
  , curr = newCurr
  }

isPressed :: Button -> Bool
isPressed = curr

justPressed :: Button -> Bool
justPressed b = not (prev b) && curr b