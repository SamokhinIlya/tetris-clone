module Game.Timer (Timer, mkTimer, tick) where

data Timer = Timer
  { timespan :: Double
  , current  :: Double
  } deriving Show

mkTimer :: Double -> Timer
mkTimer span = Timer
  { timespan = span
  , current  = span
  }

tick :: Double -> Timer -> (Timer, Bool)
tick dt t =
  let newCurrent = current t - dt
  in
    if newCurrent < 0.0
    then (t { current = timespan t }, True )
    else (t { current = newCurrent }, False)