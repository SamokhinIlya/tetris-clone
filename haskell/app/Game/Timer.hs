module Game.Timer (Timer, mkTimer, tick) where

data Timer = Timer { timespan :: Double, current :: Double }

mkTimer :: Double -> Timer
mkTimer ts = Timer { timespan = ts, current = ts }

tick :: Double -> Timer -> (Timer, Bool)
tick dt t =
  let newCurrent = current t - dt
  in if newCurrent < 0.0
    then (t { current = timespan t }, True )
    else (t { current = newCurrent }, False)