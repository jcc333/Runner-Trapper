module Runs where
import Data.List
import Data.Time.Clock
import GHC.Float
import Distances

mean numList = sum numList / genericLength numList

class Run r where
  distance :: r -> Distance
  time :: r -> DiffTime
  pace :: r -> DiffTime

data Interval = Interval Distance DiffTime deriving(Show, Eq)

instance Run Interval where
  distance (Interval d t) = d
  pace (Interval d t) =  t / ((milesNumber d) :: DiffTime)
  time (Interval d t) = t :: DiffTime

data SpeedWork = SpeedWork {speedIntervals::[Interval], 
                            jogIntervals::[Interval],
                            warmUps::[Interval]} deriving(Show, Eq)
allIntervals speedWork = 
  speedIntervals speedWork ++ jogIntervals speedWork ++ warmUps speedWork

instance Run SpeedWork where
  distance spd  = Miles $ sum $ map milesNumber $ map distance (allIntervals spd)
  time spd = sum $ map time $ allIntervals spd
  pace spd = mean $ map pace $ speedIntervals spd

newtype LongSlowDistance = LongSlowDistance Interval deriving(Show, Eq)

instance Run LongSlowDistance where
  distance (LongSlowDistance interval) = distance interval
  pace (LongSlowDistance interval) = pace interval
  time (LongSlowDistance interval) = time interval

