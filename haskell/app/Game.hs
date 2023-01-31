module Game where

import GHC.Arr
import Data.Array.IO
import Data.List
import Data.Maybe

import Canvas
import Input (Input)

import Game.Draw qualified as Draw  
import Game.Field
import Game.Input
import Game.Piece hiding (dims)
import Game.Piece qualified as Piece (dims) 
import Game.Timer

data Data = Data
  { state              :: State
  , field              :: Field
  , piece              :: Piece
  , gravityTimer       :: Timer
  , clearRowFlashTimer :: Timer
  } deriving Show

data State
  = SpawnPiece
  | Spawned { pos :: (Int, Int) }
  | BlinkDisappearingRows { doShow :: Bool, blinks :: Int }
  | ClearDisappearingRows
  | FallAfterClear
  | ChangePiece
  deriving Show

gravityTick :: Double
gravityTick = 0.5

mkData :: Data
mkData = Data
  { state = SpawnPiece
  , field = mkField
  , piece = mkPiece
  , gravityTimer = mkTimer gravityTick
  , clearRowFlashTimer = mkTimer (gravityTick / 2.0)
  }

spawnPiece :: Data -> Data
spawnPiece d = 
  let pos = (0, 5)
  in d { state = Spawned { pos = pos }, field = spawn (piece d) pos (field d) }

spawned :: Maybe Turn -> Maybe Move -> Bool -> (Int, Int) -> Data -> Data
spawned turn mov hasTicked pos d = 
  let
    (newPiece, newField, newPos) =
      let
        (piece', field', pos') = fromMaybe (piece d, field d, pos) (turn    >>= tryTurnPiece pos  (piece d) (field d))
        (field'', pos'')       = fromMaybe (defaultField, pos')    (moveDir >>= tryMovePiece pos' piece'    field'   )
          where
            (defaultField, moveDir) =
              if hasTicked
              then (amap fallingToFrozen field', Just MoveDown)
              else (field'                     , mov          )
            fallingToFrozen Falling = Frozen
            fallingToFrozen c       = c 
      in
        (piece', field'', pos'') 
    rows        = fst $ rowsCols newField
    hasFullRows = isJust . find (all (==Frozen)) $ rows
    newField'   = newField // [(Pos (y, x), Disappearing) | y <- frozenRowYs, x <- frozenRowXs]
      where
        frozenRowYs = map fst . filter (all (==Frozen) . snd) $ zip [0..] rows
        frozenRowXs = [0..snd (dims newField) - 1]
  in d
    { state =
      if hasFullRows                     then BlinkDisappearingRows { doShow = False, blinks = 4 }
      else if hasTicked && newPos == pos then ChangePiece
      else                                    Spawned { pos = newPos }
    , piece = newPiece
    , field = newField'
    }

blinkDisappearingRows :: Bool -> Int -> Double -> Data -> Data
blinkDisappearingRows doShow blinks dt d =
  if blinks > 0
  then
    let
      (clearRowFlashTimer', doFlash) = tick dt $ clearRowFlashTimer d
    in d
      { clearRowFlashTimer = clearRowFlashTimer'
      , state =
          if doFlash
          then BlinkDisappearingRows { doShow = not doShow, blinks = blinks - 1 }
          else BlinkDisappearingRows { doShow = doShow    , blinks = blinks     }
      }
  else d { state = ClearDisappearingRows }

clearDisappearingRows :: Data -> Data 
clearDisappearingRows d = 
  d { state = FallAfterClear, field = amap clear $ field d }
  where
    clear Frozen       = Falling
    clear Disappearing = Empty
    clear c            = c

fallAfterClear :: Data -> Data
fallAfterClear d =
  case tryFall $ field d of
    Nothing -> d { state = ChangePiece, field = amap fallingToFrozen (field d) }
      where
        fallingToFrozen Falling = Frozen
        fallingToFrozen c       = c
    Just field' -> d { field = field' }

update :: Data -> Canvas -> Input -> Double -> IO (Data, Canvas)
update d canvas input dt = do
  let
    mov                        = mkMove input
    turn                       = mkTurn input
    (gravityTimer', hasTicked) = tick dt (gravityTimer d)
    d' = case state d of
      SpawnPiece                               -> spawnPiece d
      Spawned { pos }                          -> spawned turn mov hasTicked pos d
      BlinkDisappearingRows { doShow, blinks } -> blinkDisappearingRows doShow blinks dt d
      ClearDisappearingRows                    -> clearDisappearingRows d
      FallAfterClear                           -> if hasTicked then fallAfterClear d else d
      ChangePiece                              -> d { state = SpawnPiece, piece = next $ piece d }

  -- draw
  Draw.clear canvas

  (_, (ch, cw)) <- getBounds canvas
  let
    cellPx = 30
    drawDisappearing =
      case state d' of
        BlinkDisappearingRows { doShow } -> doShow
        _                                -> False
    (_, Pos (fh, fw)) = bounds $ field d'
    (y0, x0) = (ch `div` 2 - (fh * cellPx) `div` 2, cw `div` 2 - (fw * cellPx) `div` 2)

  Draw.field (field d') (y0, x0) cellPx drawDisappearing canvas
  Draw.field (blueprint $ next $ piece d') (y0, x0 + fw * cellPx + cellPx) cellPx False canvas
  pure (d' { gravityTimer = gravityTimer' }, canvas)

spawn :: Piece -> (Int, Int) -> Field -> Field
spawn piece pos = copyIf (==Falling) (blueprint piece) pos (Piece.dims piece)

tryFall :: Field -> Maybe Field
tryFall field = do
  y <- find isFloating . drop 1 . reverse $ [0..height - 1]
  let ys = [0..y]
  Just $ field
    // [(Pos (y    , x), Empty             ) | y <- ys, x <- xs]
    // [(Pos (y + 1, x), field ! Pos (y, x)) | y <- ys, x <- xs]
  where
    (height, width) = dims field

    xs = [0..width - 1]

    isFloating y = any (\x -> field ! Pos (y    , x) == Falling) xs
                && all (\x -> field ! Pos (y + 1, x) == Empty  ) xs

tryMovePiece :: (Int, Int) -> Piece -> Field -> Move -> Maybe (Field, (Int, Int))
tryMovePiece pos piece field m = do
  pos' <-
    let
      (pieceHeight, pieceWidth) = Piece.dims piece
      (y', x') = pos
      (y, x) = case m of
        MoveLeft  -> (y'    , x' - 1)
        MoveRight -> (y'    , x' + 1)
        MoveDown  -> (y' + 1, x'    )
    in
      if all (inRange (bounds field) . Pos) [(y, x), (y + pieceHeight - 1, x + pieceWidth - 1)]
      then Just (y, x)
      else Nothing
  if hasCollided piece pos' field
  then Nothing
  else Just (movePiece piece pos' field, pos')

tryTurnPiece :: (Int, Int) -> Piece -> Field -> Turn -> Maybe (Piece, Field, (Int, Int))
tryTurnPiece pos piece field t =
  let
    turned = turn t piece
    (pieceHeight, pieceWidth) = Piece.dims turned
    pos' =
      let
        (y, x) = pos
        (height, width) = dims field
      in
        ( if y + pieceHeight > height then height - pieceHeight else y
        , if x + pieceWidth  > width  then width  - pieceWidth  else x
        )
  in
    if hasCollided turned pos' field
    then Nothing
    else Just (turned, movePiece turned pos' field, pos')

movePiece :: Piece -> (Int, Int) -> Field -> Field
movePiece piece pos = copyIf (==Falling) (blueprint piece) pos (Piece.dims piece) . amap fallingToEmpty
  where
    fallingToEmpty Falling = Empty
    fallingToEmpty c       = c

hasCollided :: Piece -> (Int, Int) -> Field -> Bool
hasCollided piece pos field =
  isJust . find collides $
    [ (blueprint piece ! Pos (bpY, bpX) , field ! Pos (y, x))
    | (bpY, y) <- zip [0..] [y0..y1]
    , (bpX, x) <- zip [0..] [x0..x1]
    ]
  where
    (y0, x0) = pos
    (y1, x1) = (y0 + pieceHeight - 1, x0 + pieceWidth - 1)
      where
        (pieceHeight, pieceWidth) = Piece.dims piece

    collides (Falling, Falling) = False
    collides (Falling, Empty  ) = False
    collides (Falling, _      ) = True
    collides (_      , _      ) = False