module Game where

import Canvas
import Input (Input)

import GHC.Arr

import qualified Game.Draw as Draw
import Game.Field
import Game.Input
import Game.Piece hiding (dims)
import qualified Game.Piece as Piece (dims) 
import Game.Timer

import Data.Array.IO
import Data.Bool (Bool(True))
import Data.Maybe
import Data.List

import Control.Monad
import System.Exit

data Data = Data
  { state               :: State
  , field               :: Field
  , piece               :: Piece
  , gravityTimer        :: Timer
  , clearRowFlashTimer :: Timer
  , countdown           :: Int
  } deriving Show

data State =
  SpawnPiece
  | Spawned { pos :: (Int, Int) }
  | BlinkDisappearingRows { doShow :: Bool, blinks :: Int }
  | ClearDisappearingRows
  | FallAfterClear
  | ChangePiece
  deriving (Show)

gravityTick :: Double
gravityTick = 0.5

mkData :: Data
mkData = Data
  { state = SpawnPiece
  , field = mkField
  , piece = mkPiece
  , gravityTimer = mkTimer gravityTick
  , clearRowFlashTimer = mkTimer (gravityTick / 2.0)
  , countdown = 60
  }

update :: Data -> Canvas -> Input -> Double -> IO (Data, Canvas)
update d canvas input dt = do
  let
    mov                     = mkMove input
    turn                    = mkTurn input
    (gravityTimer', hasTicked) = tick dt (gravityTimer d)
    newD = case state d of
      SpawnPiece ->
        let pos = (0, 5)
        in d { state = Spawned { pos = pos }, field = spawn (piece d) pos (field d) }
      Spawned { pos = pos } ->
        let
          (newData, newPos) =
            let (d', pos') = fromMaybe (d, pos) (turn >>= tryTurnPiece pos d)
            in if hasTicked
              then fromMaybe (d' { field = amap fallingToFrozen $ field d }, pos') (tryMovePiece pos' d' MoveDown)
              else fromMaybe (d'                                           , pos') (mov >>= tryMovePiece pos' d')
                where
                  fallingToFrozen Falling = Frozen
                  fallingToFrozen c       = c 
          (newData', hasFullRows) =
            ( newData { field = field newData // [(Pos (y, x), Disappearing) | y <- frozenRowYs, x <- frozenRowXs]}
            , isJust . find (all isFrozen) $ rows
            )
            where
              rows = fst . rowsCols $ field newData

              frozenRowYs = map fst . filter (all isFrozen . snd) $ zip [0..] rows
              frozenRowXs = [0..(snd . dims $ field newData) - 1]

              isFrozen Frozen = True
              isFrozen _      = False
        in newData'
          { state =
              if hasFullRows                     then BlinkDisappearingRows { doShow = False, blinks = 4 }
              else if hasTicked && newPos == pos then ChangePiece
              else                                    Spawned { pos = newPos }
          }
      BlinkDisappearingRows { doShow = doShow, blinks = blinks } ->
        if blinks > 0
        then
          let
            (clearRowFlashTimer', doFlash) = tick dt $ clearRowFlashTimer d
          in
            if doFlash
            then d { clearRowFlashTimer = clearRowFlashTimer', state = BlinkDisappearingRows { doShow = not doShow, blinks = blinks - 1 }}
            else d { clearRowFlashTimer = clearRowFlashTimer', state = BlinkDisappearingRows { doShow = doShow, blinks = blinks }}
        else d { state = ClearDisappearingRows }
      ClearDisappearingRows ->
        d { state = FallAfterClear, field = amap clear $ field d }
        where
          clear Frozen       = Falling
          clear Disappearing = Empty
          clear c            = c
      FallAfterClear ->
          if hasTicked
          then
            let f = tryFall $ field d
            in case f of
              Just field' -> d { state = ChangePiece, field = amap fallingToFrozen field' }
                where
                  fallingToFrozen Falling = Frozen
                  fallingToFrozen c       = c
              Nothing -> d
          else d
      ChangePiece -> d { state = SpawnPiece, piece = next $ piece d }

  -- draw
  Draw.clear canvas

  (_, (ch, cw)) <- getBounds canvas
  let
    drawDisappearing = case state newD of
      BlinkDisappearingRows { doShow = show } -> show
      _                                       -> False
    (_, Pos (fh, fw)) = bounds $ field newD
    cellPx        = 30
    (y0, x0)      = (ch `div` 2 - (fh * cellPx) `div` 2, cw `div` 2 - (fw * cellPx) `div` 2)

  Draw.field (field newD) (y0, x0) cellPx drawDisappearing canvas
  Draw.field (blueprint $ next $ piece newD) (y0, x0 + fw * cellPx + cellPx) cellPx False canvas
  let countdown' = countdown d - 1
  pure (newD { gravityTimer = gravityTimer', countdown = countdown' }, canvas)

spawn :: Piece -> (Int, Int) -> Field -> Field
spawn piece pos = copyIf isFalling (blueprint piece) pos (Piece.dims piece)
  where
    isFalling Falling = True
    isFalling _       = False

tryFall :: Field -> Maybe Field
tryFall field =
  let
    xs = [0..width - 1]
    ys = [y | y <- drop 1 . reverse $ [0..height - 1]
            , let
                dstIsFree = all (isEmpty . \x -> field ! Pos (y + 1, x)) xs
                srcHasFalling = any (isFalling . \x -> field ! Pos (y, x)) xs
              in
                dstIsFree && srcHasFalling                                 ]
  in
    if null ys
    then Nothing
    else Just $ field
      // [(Pos (y    , x), Empty             ) | y <- ys, x <- xs]
      // [(Pos (y + 1, x), field ! Pos (y, x)) | y <- ys, x <- xs]
  where
    (height, width) = dims field

    isEmpty Empty = True
    isEmpty _     = False

    isFalling Falling = True
    isFalling _       = False

tryMovePiece :: (Int, Int) -> Data -> Move -> Maybe (Data, (Int, Int))
tryMovePiece pos d m = do
    pos' <-
      let
        (pieceHeight, pieceWidth) = Piece.dims $ piece d
        (y', x') = pos
        (y, x) = case m of
          MoveLeft  -> (y'    , x' - 1)
          MoveRight -> (y'    , x' + 1)
          MoveDown  -> (y' + 1, x'    )
      in
        if all (inRange $ bounds $ field d) [Pos (y, x), Pos (y + pieceHeight - 1, x + pieceWidth - 1)]
        then Just (y, x)
        else Nothing
    if hasCollided (piece d) pos' (field d)
    then Nothing
    else Just (d { field = movePiece (piece d) pos' (field d) }, pos')

tryTurnPiece :: (Int, Int) -> Data -> Turn -> Maybe (Data, (Int, Int))
tryTurnPiece pos d t =
  let
    turned = turn t $ piece d
    (pieceHeight, pieceWidth) = Piece.dims turned
    newPos =
      let
        (y, x) = pos
        (height, width) = dims $ field d
      in
        ( if y + pieceHeight > height then height - pieceHeight else y
        , if x + pieceWidth  > width  then width  - pieceWidth  else x
        )
  in
    if hasCollided turned newPos (field d)
    then Nothing
    else Just (d { field = movePiece turned newPos (field d), piece = turned }, newPos)

movePiece :: Piece -> (Int, Int) -> Field -> Field
movePiece piece pos = copyIf isFalling (blueprint piece) pos (Piece.dims piece) . amap fallingToEmpty
  where
    isFalling Falling = True
    isFalling _       = False

    fallingToEmpty Falling = Empty
    fallingToEmpty c       = c

hasCollided :: Piece -> (Int, Int) -> Field -> Bool
hasCollided piece pos field =
  isJust . find collides
    $ [(blueprint piece ! Pos (bpY, bpX)
      , field           ! Pos (y, x)) | (bpY, y) <- zip [0..3] [y0..y1]
                                      , (bpX, x) <- zip [0..3] [x0..x1]]
  where
    (y0, x0) = pos
    (y1, x1) = (y0 + pieceHeight - 1, x0 + pieceWidth - 1)
      where
        (pieceHeight, pieceWidth) = Piece.dims piece

    collides (Falling, Falling) = False
    collides (Falling, Empty  ) = False
    collides (Empty  , Falling) = False
    collides (Empty  , Empty  ) = False
    collides (_      , _      ) = True