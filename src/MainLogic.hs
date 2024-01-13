module MainLogic
where

import Data.Text (pack, unpack)
import qualified System.Environment as Env

import qualified Options as Opt
import Commands as Cmd


runWithOptions :: Opt.CliOptions -> Opt.FileOptions -> IO ()
runWithOptions cliOptions fileOptions = do
  -- putStrLn $ "@[runWithOptions] cliOpts: " <> show cliOptions
  -- putStrLn $ "@[runWithOptions] fileOpts: " <> show fileOptions
  case cliOptions.job of
    Nothing -> do
      putStrLn "@[runWithOptions] start on nil command."
    Just aJob -> do
      -- Get environmental context in case it's required in the merge. Done here to keep the merge pure:
      mbHome <- Env.lookupEnv "ParserHOME"
      let
        envOptions = Opt.EnvOptions {
            home = pack <$> mbHome
            -- TODO: put additional env vars.
          }
        rtOptions = Opt.mergeOptions cliOptions fileOptions envOptions 
        -- switchboard to command executors:
        cmdExecutor =
          case aJob of
            Opt.HelpCmd -> Cmd.helpHu
            Opt.VersionCmd -> Cmd.versionHu
            Opt.ParseCmd aPath algo -> Cmd.parseCmd (unpack aPath) algo
      result <- cmdExecutor rtOptions
      -- TODO: return a properly kind of conclusion.
      pure ()
