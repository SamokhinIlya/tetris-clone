module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

import Data.ByteString (ByteString, pack)

import Data.Array

import qualified Game

import Canvas (Canvas)
import Input

main :: IO ()
main = play window background updatesPerSecond d render handleEvent update
    where d                = mkData (1280, 720)
          window           = InWindow "title" (width d, height d) (100, 100)
          background       = black
          updatesPerSecond = 60

data Data = Data
    { canvas :: Canvas
    , width :: Int
    , height :: Int
    , gameData :: Game.Data
    , input :: Input
    }

mkData :: (Int, Int) -> Data
mkData (w, h) = Data
    { canvas = array ((0, 0), (h - 1, w - 1)) [((y, x), color) | y <- [0..(h - 1)]
                                                               , x <- [0..(w - 1)] ]
    , width = w
    , height = h
    , gameData = Game.mkData
    , input = Input
    }
    where color = (0xFF, 0x00, 0x00, 0x00)

render :: Data -> Picture
render d = bitmapOfByteString (width d) (height d) format byteString False
    where
        format                   = BitmapFormat TopToBottom PxABGR
        byteString               = (pack . concatMap pixelToList . elems . canvas) d
        pixelToList (a, b, g, r) = [a, b, g, r]

handleEvent :: Event -> Data -> Data
handleEvent event d = d

update :: Float -> Data -> Data
update dt d
    = d
    { canvas = newCanvas
    , gameData = newGameData
    }
    where (newGameData, newCanvas) = Game.update (gameData d) (canvas d) (input d) dt