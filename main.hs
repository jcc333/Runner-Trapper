module Main where
import Distances
import Runs

yesterday = SpeedWork [Interval (Miles 1) 6, Interval (Miles 1) 7] [] [Interval (Miles 1) 8]

main = putStrLn $ show yesterday
