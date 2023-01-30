{-# LANGUAGE TupleSections #-}

module Input
  ( Input, mkInput, mouse, keyboard
  , Mouse, MouseKey(..)
  , Keyboard, KBKey(..)
  , Button, mkButton, update, isPressed, justPressed
  ) where

import Data.Map.Strict (Map, fromList)

data Input = Input
  { mouse    :: Mouse
  , keyboard :: Keyboard
  } deriving (Show)

mkInput :: Input
mkInput = Input
  { mouse = mkMouse
  , keyboard = mkKeyboard
  }

type Mouse = Map MouseKey Button

data MouseKey
  = MouseLeft
  | MouseRight
  deriving (Enum, Bounded, Eq, Ord, Show)

mkMouse :: Mouse
mkMouse = fromList $ map (, mkButton) ([minBound..maxBound] :: [MouseKey])

type Keyboard = Map KBKey Button

data KBKey
  = KBLeft
  | KBRight
  | KBDown
  deriving (Enum, Bounded, Eq, Ord, Show)

mkKeyboard :: Keyboard
mkKeyboard = fromList $ map (, mkButton) ([minBound..maxBound] :: [KBKey])

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