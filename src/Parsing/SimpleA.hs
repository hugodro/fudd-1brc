{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE InstanceSigs #-}

module Parsing.SimpleA (simpleParser) where

import Control.Monad ( (<$!>), foldM )
import Control.Concurrent.Async (forConcurrently)

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char ( isDigit )
import Data.Int ( Int64 ) 
import Data.Word ( Word8 )
import Data.Text (Text, unpack, foldl')
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Map as Mp

import GHC.Conc (numCapabilities)
import System.Posix.Files ( getFileStatus, fileSize )
import System.IO ( hSeek, openBinaryFile, SeekMode(AbsoluteSeek), IOMode(ReadMode) )

import qualified Options.Runtime as Rto
import System.Posix (ControlCharacter(EndOfLine))

data StationValue = StationValue {
    minVal ::  {-# UNPACK #-} !Int64
    , maxVal ::  {-# UNPACK #-} !Int64
    , totalInt ::  {-# UNPACK #-} !Int64
    , totalDec ::  {-# UNPACK #-} !Int64
    , count ::  {-# UNPACK #-} !Int64
  }

instance Show StationValue where
  show :: StationValue -> String
  show sv = "min:" <> show sv.minVal <> ", max: " <> show sv.maxVal
      <> ", avg: " <> show (countAvg sv.totalInt sv.totalDec sv.count)

countAvg :: Int64 -> Int64 -> Int64 -> Float
countAvg tI tD c =
      let
        ntI = tI + tD `div` 10
        ntD = tD `mod` 10
      in ((fromIntegral ntI :: Float) + (fromIntegral ntD :: Float ) / 10.0) / (fromIntegral c :: Float)


instance Semigroup StationValue where
  (<>) :: StationValue -> StationValue -> StationValue
  (StationValue !nV !xV !tI !tD !c) <> (StationValue !nV' !xV' !tI' !tD' !c') =
    StationValue (min nV nV') (max xV xV') (tI + tI') (tD + tD') (c + c')

instance Monoid StationValue where
  mempty :: StationValue
  mempty = StationValue 1000 (-1000) 0 0 0

type SvAccum = Mp.Map String StationValue

data EdgeCase =
  ShortLine !BL.ByteString
  | EndOfBuffer !String
  | InvalidInt !Char
  | InvalidDecimal !Char


simpleParser :: FilePath -> IO ()
simpleParser inPath = do
  fileSize <- fromIntegral . fileSize <$> getFileStatus inPath
  let
    numCap64 = fromIntegral numCapabilities :: Int64
    chunkSize = fileSize `div` numCap64

  inFile <- BL.readFile inPath
  rezA <-
    forConcurrently [0..numCap64-1] (parserOffset inPath numCap64 chunkSize)
    --  parseChunky chunkSize 0 inFile
  explainResult rezA
  return ()
  where
    parserOffset :: FilePath -> Int64 -> Int64 -> Int64 -> IO SvAccum
    parserOffset fp numCap64 cSize n = do
      let
        limiter = if n < numCap64 - 1 then BL.take cSize else id
        offset = fromIntegral (n * cSize)
      fileHandle <- openBinaryFile fp ReadMode
      hSeek fileHandle AbsoluteSeek offset
      rezA <- scanLine (Mp.empty). limiter <$!> BL.hGetContents fileHandle
      case rezA of
        Left (map, errCond) -> pure map
        Right (!map, !r) -> pure map
      -- parseBS . limiter <$!> BL.hGetContents fileHandle
{-# INLINE simpleParser #-}


parseChunky :: Int64 -> Int64 -> BL.ByteString -> Int64
parseChunky !chunkSize !lines !stream =
  let
    !aChunk = BL.take chunkSize stream
  in
  if BL.length aChunk < chunkSize then
    lines + parseBS aChunk
  else
    let
      partLength = parseBS aChunk
    in
    parseChunky chunkSize (lines + partLength) (BL.drop chunkSize stream)
{-# INLINE parseChunky #-}


parseBS :: BL.ByteString -> Int64
parseBS !stream =
  let
    (!lines, !leftOver) = BL.foldl' parseChar (0, 0) stream
  in
    lines
{-# INLINE parseBS #-}


parseChar :: (Int64, Int64) -> Char -> (Int64, Int64)
parseChar (!lines, !run) ch =
  if ch == '\n' then
    (lines + 1, 0)
    -- <> [ unpack . decodeUtf8 . BL.toStrict . BL.pack $ run ]
  else
    (lines, run + 1)
{-# INLINE parseChar #-}


scanLine :: SvAccum -> BL.ByteString -> Either (SvAccum, EdgeCase) (SvAccum, BL.ByteString)
scanLine !accum !stream =
  case pStation "" stream of
    Left !aCond -> Left (accum, aCond)
    Right (!aLabel, !rest) ->
      case pInteger 0 rest of
        Left bCond -> Left (accum, bCond)
        Right (!anInt, !rest) ->
          case pDecimal 0 rest of
            Left cCond -> Left (accum, cCond)
            Right (!aDec, !rest) ->
              let
                !mbSV = Mp.lookup aLabel accum
                !nSV = StationValue anInt anInt anInt aDec 1
                !nAccum = case mbSV of
                  Nothing ->
                    Mp.insert aLabel nSV accum
                  Just !oSV -> 
                    Mp.insert aLabel (oSV <> nSV) accum
              in
              scanLine nAccum rest
{-# INLINE scanLine #-}


pStation :: [Char] -> BL.ByteString -> Either EdgeCase ([Char], BL.ByteString)
pStation !accum !stream =
  case BL.uncons stream of
    Nothing -> Left $ EndOfBuffer accum
    Just !aValue ->
      case aValue of
        ('\n', !rest) -> Left $ ShortLine rest
        (';', !rest) -> Right (accum, rest)
        (!h, !rest) -> pStation (h : accum) rest
{-# INLINE pStation #-}

chrToInt :: Char -> Int64
chrToInt !c =
  case c of
    '0' -> 0
    '1' -> 1
    '2' -> 2
    '3' -> 3
    '4' -> 4
    '5' -> 5
    '6' -> 6
    '7' -> 7
    '8' -> 8
    '9' -> 9
    _ -> 0
{-# INLINE chrToInt #-}

pInteger :: Int64 -> BL.ByteString -> Either EdgeCase (Int64, BL.ByteString)
pInteger !accum !stream =
  case BL.uncons stream of
    Nothing -> Left $ EndOfBuffer $ show accum
    Just !aValue ->
      case aValue of
        ('\n', rest) -> Left $ ShortLine rest
        ('.', rest) -> Right (accum, rest)
        (!h, !rest) ->
          if isDigit h then
            pInteger (accum * 10 + chrToInt h) rest
          else
            Left $ InvalidInt h
{-# INLINE pInteger #-}

pDecimal :: Int64 -> BL.ByteString -> Either EdgeCase (Int64, BL.ByteString)
pDecimal !accum !stream =
  case BL.uncons stream of
    Nothing -> Left $ EndOfBuffer $ show accum
    Just !aValue ->
      case aValue of
        ('\n', !rest) -> Right (accum, rest)
        (!h, !rest) ->
          if isDigit h then
            pDecimal (accum * 10 + chrToInt h) rest
          else
            Left $ InvalidDecimal h
{-# INLINE pDecimal #-}


explainResult :: [SvAccum] -> IO ()
explainResult !sValues = do
  let
    !mergedValues = Prelude.foldl (\accum nm -> Mp.unionWith (<>) accum nm) Mp.empty sValues
    !result = Mp.foldlWithKey (\accum k val -> let strVal = reverse k <> ": " <> show val in if accum == "" then strVal else accum <> ", " <> strVal) "" mergedValues
    totalVal = Mp.foldl' (\accum val -> accum + val.count) 0 mergedValues
  -- putStrLn $ "@[explainResult] stations: " <> result
  putStrLn $ "@[explainResult] # of stations: " <> show totalVal

