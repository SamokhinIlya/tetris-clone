module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

main :: IO ()
main = play (InWindow "title" (1280, 720) (100, 100)) black updatesPerSecond mkData render handleEvent update

updatesPerSecond :: Int
updatesPerSecond = 60

data Data = Data

mkData :: Data
mkData = Data

render :: Data -> Picture
render = const blank

handleEvent :: Event -> Data -> Data
handleEvent event d = d

update :: Float -> Data -> Data
update dt d = d