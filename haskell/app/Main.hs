{-# LANGUAGE BlockArguments #-}

module Main (main) where

import Prelude hiding (lookup)

import GHC.Float
import Control.Monad
import Data.Array.Storable
import Data.ByteString (ByteString, pack)
import Data.Map.Strict (Map, empty, insertWith, mapWithKey, lookup)
import Data.Maybe

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Word
import Foreign.Ptr
import Foreign.ForeignPtr

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Interface.IO.Game

import qualified Game

import Canvas (Canvas, Pixel)
import qualified Canvas

import Input hiding (update)
import qualified Input (update)

main :: IO ()
main = do
  d <- mkData (1280, 720)
  let
    window           = InWindow "title" (width d, height d) (100, 100)
    background       = green
    updatesPerSecond = 60
  playIO window background updatesPerSecond d render handleEvent update

data Data = Data
  { canvas        :: Canvas
  , width         :: Int
  , height        :: Int
  , gameData      :: Game.Data
  , input         :: Input
  , keyboardState :: Map KBKey (NonEmpty KeyState)
  , mouseState    :: Map MouseKey (NonEmpty KeyState)
  }

mkData :: (Int, Int) -> IO Data
mkData (w, h) = do
  canvas <- newArray ((0, 0), (h - 1, w - 1)) Canvas.black
  pure Data
    { canvas        = canvas 
    , width         = w
    , height        = h
    , gameData      = Game.mkData
    , input         = mkInput
    , keyboardState = empty
    , mouseState    = empty
    }

render :: Data -> IO Picture
render d = do
  withStorableArray (canvas d) toBitmap
  where
    toBitmap ptr = do
      let format = BitmapFormat TopToBottom PxRGBA
      foreignPtr <- newForeignPtr_ (castPtr ptr :: Ptr Word8)
      pure $ bitmapOfForeignPtr (width d) (height d) format foreignPtr False

handleEvent :: Event -> Data -> IO Data
handleEvent event d = pure $ case event of
  EventKey (SpecialKey  key) state _ _ -> fromMaybe d $ updateKeyboardState key state
  EventKey (MouseButton key) state _ _ -> fromMaybe d $ updateMouseState key state
  _                                    -> d
  where
    updateKeyboardState :: SpecialKey -> KeyState -> Maybe Data
    updateKeyboardState key state = do
      k <- case key of
        KeyDown  -> Just KBDown
        KeyLeft  -> Just KBLeft
        KeyRight -> Just KBRight
        _        -> Nothing
      Just d { keyboardState = insertWith NonEmpty.append k (NonEmpty.singleton state) (keyboardState d) }

    updateMouseState :: MouseButton -> KeyState -> Maybe Data
    updateMouseState key state = do
      k <- case key of
        LeftButton  -> Just MouseLeft
        RightButton -> Just MouseRight
        _           -> Nothing
      Just d { mouseState = insertWith NonEmpty.append k (NonEmpty.singleton state) (mouseState d) }

update :: Float -> Data -> IO Data
update dt d = do
  let input' = updateInput d
  (gameData', canvas') <- Game.update (gameData d) (canvas d) input' (float2Double dt)
  pure d
    { canvas = canvas'
    , gameData = gameData'
    , input = input'
    , keyboardState = empty
    , mouseState = empty
    }
  where
    updateInput d = (input d)
      { keyboard = mapWithKey (updateKey $ keyboardState d) (keyboard $ input d)
      , mouse    = mapWithKey (updateKey $ mouseState    d) (mouse    $ input d)
      }
      where
        updateKey events k b = maybe (Input.update (isPressed b) b) (foldr (Input.update . asBool) b) (lookup k events)

        asBool Down = True
        asBool Up   = False