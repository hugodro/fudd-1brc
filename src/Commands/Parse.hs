{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE InstanceSigs #-}

module Commands.Parse where

import Data.Text (Text, unpack)

import qualified Options.Runtime as Rto
import System.Posix (ControlCharacter(EndOfLine))
import Parsing.SimpleA (simpleParser)


parseCmd :: FilePath -> Text -> Rto.RunOptions -> IO ()
parseCmd inPath algo rtOpts = do
  putStrLn "@[parseCmd] starting."
  case algo of
    "main" -> simpleParser inPath
    _ -> putStrLn $ "@[parseCmd]: unknown algo: " <> unpack algo
