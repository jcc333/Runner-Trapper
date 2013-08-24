module Distances where

mileToKmConversionRate = 1.60934
meterToKmConversionRate = 1000


data Distance = Miles Double
              | Meters Double
              | Kilometers Double

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

toDouble distance = case distance of
  Miles n -> n
  Meters n -> n
  Kilometers n -> n

milesDouble = toDouble.toMiles

metersDouble = toDouble.toMeters

kilometersDouble = toDouble.toKilometers

instance Eq Distance where
  (==) (Meters a) (Meters b) = a == b
  (==) left right = (toMeters left) == (toMeters right)

instance Ord Distance where
  compare left right = compare leftDouble rightDouble
    where leftDouble = metersDouble left
          rightDouble = metersDouble right

instance Show Distance where
  show distance = case distance of
    Miles n -> (show n) ++ " miles"
    Kilometers n -> (show n) ++ " kilometers"
    Meters n -> (show n) ++ " meters"

