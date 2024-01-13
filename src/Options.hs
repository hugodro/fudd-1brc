module Options  (
  module Options.Cli
  , module Options.ConfFile
  , module Options.Runtime
  , mergeOptions
 )
where

import qualified Data.Text as DT
import qualified Data.Text.Encoding as DT

import Options.Cli
import Options.ConfFile
import Options.Runtime


mergeOptions :: CliOptions -> FileOptions -> EnvOptions -> RunOptions
mergeOptions cli file env =
  -- TODO: put proper priority filling of values for the Runtime Options.
  let
    defO = defaultRun
    -- Update from config file:
    fileO =
      let
        dbgO = case file.debug of
          Nothing -> defO
          Just aVal -> defO { debug = aVal } :: RunOptions
        {- HERE: add additional configurations:
        Eg: rootO = case file.rootDir of
          Nothing -> dbO
          Just aVal -> dbO { root = DT.pack aVal } :: RunOptions
        -}
      in
      dbgO
    -- TODO: update from CLI options
    cliO = case cli.debug of
      Nothing -> fileO
      Just aVal -> fileO { debug = aVal } :: RunOptions
    -- TODO: update from ENV options
    envO = cliO
  in 
  envO
