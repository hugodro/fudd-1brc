module Main where

import qualified Control.Exception as Cexc
import qualified System.Environment as Senv
import qualified System.IO.Error as Serr

import qualified Options as Opt
import qualified MainLogic as Ml


main :: IO ()
main = do
  eiOptions <- Opt.parseCliOptions
  case eiOptions of
    Left errMsg ->
      putStrLn $ "err: " <> errMsg
    Right cliOptions -> do
      mbFileOptions <- case cliOptions.configFile of
          Nothing -> do
            eiEnvConfFile <- Cexc.try $ Senv.getEnv "ParserCONF" :: IO (Either Serr.IOError String)
            case eiEnvConfFile of
              Left _ -> do
                eiConfPath <- Opt.defaultConfigFilePath
                case eiConfPath of
                  Left err -> pure . Left $ "@[main] defaultConfigFilePath err: " <> err
                  Right confPath -> Opt.parseFileOptions confPath
              Right aPath -> Opt.parseFileOptions aPath
          Just aPath -> Opt.parseFileOptions aPath
      case mbFileOptions of
        Left errMsg -> putStrLn $ "err: " <> errMsg
        Right fileOptions ->
          Ml.runWithOptions cliOptions fileOptions >> pure ()
