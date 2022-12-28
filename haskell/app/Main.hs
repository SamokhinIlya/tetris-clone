{-# LANGUAGE BlockArguments #-}
module Main(main) where

import System.Exit

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
  EventKey (SpecialKey key) state _ _ -> updKey key state
  _                                   -> pure d
  where
    updKey :: SpecialKey -> KeyState -> IO Data
    updKey key state = do
      when (key `elem` [KeyDown, KeyLeft, KeyRight]) $
        print (key, state)
      pure $ updKeyboard case key of
        KeyDown  -> kb { down  = updated down  }
        KeyLeft  -> kb { left  = updated left  }
        KeyRight -> kb { right = updated right }
        _        -> kb
      where 
        updKeyboard new = d
          { input = (input d)
            { keyboard = new 
            }
          }
        updated button = Input.update (asBool state) . button $ kb
          where
            asBool Down = True
            asBool Up   = False
        kb = keyboard . input $ d

update :: Float -> Data -> IO Data
update dt d = do
  (newGameData, newCanvas) <- Game.update (gameData d) (canvas d) (input d) dt
  pure d { canvas = newCanvas , gameData = newGameData }