{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}

module Main(main) where

import System.Exit

import GHC.Float

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Interface.IO.Game

import Data.ByteString (ByteString, pack)

import Data.Array.Storable
import Data.Word
import Foreign.Ptr
import Foreign.ForeignPtr

import qualified Game

import Canvas (Canvas, Pixel)
import qualified Canvas
import qualified Input(update)
import Input hiding (update)
import Control.Monad (when)

import qualified Data.Map.Strict as Map
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty

main :: IO ()
main = do
  d <- mkData (1280, 720)
  let
    window           = InWindow "title" (width d, height d) (100, 100)
    background       = green
    updatesPerSecond = 60
  playIO window background updatesPerSecond d render handleEvent update

data Data = Data
  { canvas :: Canvas
  , width :: Int
  , height :: Int
  , gameData :: Game.Data
  , input :: Input
  , keyboardState :: Map.Map KBKey (NonEmpty KeyState)
  , mouseState :: Map.Map MouseKey (NonEmpty KeyState)
  }

mkData :: (Int, Int) -> IO Data
mkData (w, h) = do
  canvas <- newArray ((0, 0), (h - 1, w - 1)) Canvas.black
  pure Data
    { canvas = canvas 
    , width = w
    , height = h
    , gameData = Game.mkData
    , input = mkInput
    , keyboardState = Map.empty
    , mouseState = Map.empty
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
handleEvent event d = case event of
  EventKey (SpecialKey key) state _ _ ->
    let
      kk = case key of
        KeyDown  -> Just KBDown
        KeyLeft  -> Just KBLeft
        KeyRight -> Just KBRight
        _        -> Nothing
    in
      case kk of
        Just k -> pure d { keyboardState = Map.insertWith NonEmpty.append k (NonEmpty.singleton state) (keyboardState d) }
        Nothing -> pure d
  EventKey (MouseButton key) state _ _ ->
    let
      kk = case key of
        LeftButton  -> Just MouseLeft
        RightButton -> Just MouseRight
        _           -> Nothing
    in
      case kk of
        Just k -> pure d { mouseState = Map.insertWith NonEmpty.append k (NonEmpty.singleton state) (mouseState d) }
        Nothing -> pure d
  _ -> pure d

update :: Float -> Data -> IO Data
update dt d = do
  let input' = updateInput d
  (gameData', canvas') <- Game.update (gameData d) (canvas d) input' (float2Double dt)
  pure d { canvas = canvas' , gameData = gameData', input = input', keyboardState = Map.empty, mouseState = Map.empty }
  where
    updateInput d = (input d)
      { keyboard = Map.mapWithKey (updateKey $ keyboardState d) (keyboard $ input d)
      , mouse = Map.mapWithKey (updateKey $ mouseState d) (mouse $ input d)
      }
        where
          updateKey events k b = maybe (Input.update (isPressed b) b) (foldr (Input.update . asBool) b) (Map.lookup k events)

          asBool Down = True
          asBool Up   = False