{-# LANGUAGE OverloadedStrings #-}

import Control.Semigroup ((<>))

import qualified Data.Map as Mp
import qualified Data.Map.Strict as Ms
import qualified Data.Text as T
import qualified Data.Text.IO as T

data Station = Station {
    frequency :: Int
    , baseTemp :: Double
  } deriving (Show)

instance Semigroup Station where
  (Station n1 f1 t1) <> (Station n2 f2 t2) =
    Station n1 (f1 + f2) ((t1 * fromIntegral f1 + t2 * fromIntegral f2) / fromIntegral (f1 + f2))


readRef = do
  rawData <- T.readFile "weather_stations.csv"
  let
    lines = T.splitOn "\n" rawData
    weatherData = Prelude.foldl
      (\accum l -> Ms.insertWith (<>) (splitLine l) 1 accum)
      (Mp.empty :: Mp.Map T.Text Int) lines
  print weatherData

splitLine :: T.Text -> T.Text
splitLine = head . T.splitOn ";" 


