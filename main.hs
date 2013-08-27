module Main where
import Distances
import Runs
import TimeConversion

yesterday = SpeedWork [Interval (Miles 1)  6, Interval (Miles 1) 7] [] [Interval (Miles 1) 8]

tomorrow = LongSlowDistance $ Interval (Miles 13) (minutes 80)

main = putStrLn $ show yesterday
