#! /usr/bin/env stack 
{-
  stack script
  --resolver lts-21.25 --system-ghc
  --package random --package text --package containers
  --package async --package process
  --optimize
  --ghc-options -threaded 
  -- +RTS -N -s -RTS
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE BangPatterns #-}

import Control.Concurrent.Async (forConcurrently)
import GHC.Conc (numCapabilities)

import qualified Data.Map as Mp
import qualified Data.Map.Strict as Ms
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Monoid (Monoid, (<>))

import System.Random (randomRIO)
import System.Process (shell)

data Station = Station {
    frequency :: Int
    , baseTemp :: Double
  } deriving (Show)

instance Monoid Station where
  mempty = Station 0 0
  mappend = (<>)


instance Semigroup Station where
  (Station f1 t1) <> (Station f2 t2) =
    Station (f1 + f2) ((t1 * fromIntegral f1 + t2 * fromIntegral f2) / fromIntegral (f1 + f2))


readRef fileName = do
  rawData <- T.readFile fileName
  let
    lines = T.splitOn "\n" rawData
    weatherData = Prelude.foldl
      (\accum l -> 
        let
          pieces = T.splitOn ";" l
        in
        if length pieces > 1 then
          let
            label = head pieces
            temp = read (T.unpack (pieces !! 1)) :: Double
          in
          Ms.insertWith (<>) label (Station 1 temp) accum
        else
          accum
      )
      (Mp.empty :: Mp.Map T.Text Station) lines
  return weatherData

makeNthName prefix nth = prefix <> "_" <> (show nth) <> ".txt"

genFile fileNamePrefix baseData numLines seqNbr = do
  -- putStrLn $ "@[genFile] seq: " <> (show seqNbr) <> ", nbrLines: " <> (show numLines) <> "."
  let
    !keys = Mp.keys baseData
    numKeys = length keys
    fileName = makeNthName fileNamePrefix seqNbr
  !lines <- sequence $ replicate numLines (genLine keys numKeys)
  T.writeFile fileName (T.concat lines)
  where
  genLine keys numKeys = do
    key <- randomRIO (0, numKeys - 1)
    newTemp <- randomRIO (0, 40) :: IO Double
    -- putStrLn $ "@[genLine] key: " <> (show key) <> ", temp: " <> (show newTemp) <> "."
    let
      label = keys !! key
      -- (Station f t) = baseData Mp.! label
      fmtTemp = (fromIntegral (floor (newTemp * 100)) :: Double) / 100.0
    return $ T.concat [label, ";", T.pack $ show fmtTemp, "\n"]


main = do
  let
    targetSize = 1000000 :: Int
    numCap = fromIntegral numCapabilities :: Int
    chunkSize = targetSize `div` numCap
    fileNamePrefix = "test_1"
  !baseData <- readRef "weather_stations.csv"
  putStrLn $ "@[main] genTest, " <> (show numCap) <> " threads, " <> (show targetSize) <> " lines."
  forConcurrently [0..numCap-1] (genFile fileNamePrefix baseData chunkSize)
  putStrLn $ "cat " <> (foldl (\accum n -> let fn = makeNthName fileNamePrefix n in if accum == "" then fn else accum <> " " <> fn) "" [0..numCap-1]) <> " >" <> fileNamePrefix <> ".txt"


