module Main (main) where

import Prelude hiding (lookup)

import GHC.Float
import Data.Array.Storable
import Data.ByteString (ByteString, pack)
import Data.Map.Strict (Map, empty, insertWith, mapWithKey, lookup)
import Data.Maybe

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Word
import Foreign.Ptr
import Foreign.ForeignPtr

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Interface.IO.Game

import Game qualified

import Canvas (Canvas, Pixel)
import Canvas qualified

import Input hiding (update)
import Input qualified (update)

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
  , keyboardState :: Map KbKey (NonEmpty KeyState)
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
        KeyDown  -> Just KbDown
        KeyLeft  -> Just KbLeft
        KeyRight -> Just KbRight
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
        updateKey events key prev =
          let defaultButton = Input.update (isPressed prev) prev
          in
            maybe defaultButton (foldr (Input.update . (==Down)) prev) (lookup key events)