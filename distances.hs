module Distances where
import Data.Time

mileToKmConversionRate = 1.60934
meterToKmConversionRate = 1000

data Distance = Miles DiffTime
              | Meters DiffTime
              | Kilometers DiffTime

toMiles distance = case distance of
  Miles n -> Miles n
  Meters n -> Miles $ n / (meterToKmConversionRate * mileToKmConversionRate)
  Kilometers n -> Miles $ n / mileToKmConversionRate

toKilometers distance = case distance of
  Miles n -> Kilometers $ n * mileToKmConversionRate
  Meters n -> Kilometers $ n / meterToKmConversionRate
  Kilometers n -> Kilometers n

toMeters distance = case distance of
  Miles n -> Meters $ n * mileToKmConversionRate * meterToKmConversionRate
  Meters n -> Meters n
  Kilometers n -> Meters $ n * meterToKmConversionRate

toNumber distance = case distance of
  Miles n -> n
  Meters n -> n
  Kilometers n -> n

milesNumber = toNumber.toMiles

metersNumber = toNumber.toMeters

kmNumber = toNumber.toKilometers

instance Eq Distance where
  (==) (Meters a) (Meters b) = a == b
  (==) left right = (metersNumber left) == (metersNumber right)

instance Ord Distance where
  compare left right = compare leftNumber rightNumber
    where leftNumber = metersNumber left
          rightNumber = metersNumber right

instance Show Distance where
  show distance = case distance of
    Miles n -> (show n) ++ " miles"
    Kilometers n -> (show n) ++ " kilometers"
    Meters n -> (show n) ++ " meters"

