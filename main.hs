{-# LANGUAGE TemplateHaskell, 
  OverloadedStrings,
  TypeFamilies,
  QuasiQuotes#-}
module Main where
import Distances
import Runs
import TimeConversion
import Yesod

data RunnerTrapper = RunnerTrapper SpeedWork

mkYesod "RunnerTrapper" [parseRoutes|
/ HomeR GET
|]

instance Yesod RunnerTrapper

yesterday = SpeedWork [Interval (Miles 1)  6, Interval (Miles 1) 7] [] [Interval (Miles 1) 8]

tomorrow = LongSlowDistance $ Interval (Miles 13) (minutes 80)

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|<p>Yesterday: #{show yesterday}
                                  <p>Tomorrow: #{show tomorrow}|]

main :: IO ()
main = warp 3000 $ RunnerTrapper yesterday
