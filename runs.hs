module Runs where
import Data.List
import GHC.Float
import Distances
mean numList = sum numList / genericLength numList

class Run r where
  distance :: r -> Distance
  time :: r -> Double
  pace :: r -> Double

data Interval = Interval Distance Double deriving(Show, Eq)

instance Run Interval where
  distance (Interval d t) = d
  pace (Interval d t) =  t / milesDouble d
  time (Interval d t) = t

data SpeedWork = SpeedWork {speedIntervals::[Interval], 
                            jogIntervals::[Interval],
                            warmUps::[Interval]} deriving(Show, Eq)
allIntervals speedWork = 
  speedIntervals speedWork ++ jogIntervals speedWork ++ warmUps speedWork

instance Run SpeedWork where
  distance spd  = Miles $ sum $ map milesDouble $ map distance (allIntervals spd)
  
  time spd = sum $ map time $ allIntervals spd
  
  pace spd = mean $ map pace $ speedIntervals spd

