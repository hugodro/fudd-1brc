{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Options.ConfFile where

import qualified Control.Exception as Cexc
import qualified Data.Aeson as Aes
import Data.Text (Text)
import GHC.Generics
import qualified System.Directory as Sdir
import qualified System.Environment as Senv
import qualified System.FilePath.Posix as Spsx
import qualified System.IO.Error as Serr

import qualified Data.Yaml as Yaml



data FileOptions = FileOptions {
  debug :: Maybe Int
  , primaryLocale :: Maybe String
  -- HERE: add new parameters received from the config file:
  -- Et: , rootDir :: Maybe String
 }
 deriving stock (Show, Generic)


defaultConfName :: FilePath
defaultConfName = "config.yaml"


defaultConfigFilePath :: IO (Either String FilePath)
defaultConfigFilePath = do
  eiHomeDir <- Cexc.try $ Sdir.getHomeDirectory :: IO (Either Serr.IOError FilePath)
  case eiHomeDir of
    Left err -> pure . Left $ "@[defaultConfigFilePath] err: " <> show err
    Right aPath -> pure . Right $ Spsx.joinPath [aPath, defaultConfName]


-- YAML support:
instance Aes.FromJSON FileOptions

parseFileOptions :: FilePath -> IO (Either String FileOptions)
parseFileOptions filePath =
  let
    fileExt = Spsx.takeExtension filePath
  in case fileExt of
    ".yaml" -> do
      eiRez <- Yaml.decodeFileEither filePath
      case eiRez of
        Left err -> pure . Left $ "@[parseYaml] err: " <> show err
        Right aContent ->
          {- HERE: add new parameters processing:
          Eg:
          case aContent.rootDir of
            Nothing -> pure $ Right aContent
            Just aVal -> case head aVal of
                '$' -> do
                  eiEnvValue <- Cexc.try $ Senv.getEnv (tail aVal) :: IO (Either Serr.IOError String)
                  case eiEnvValue of
                    Left err -> pure . Right $ aContent { rootDir = Nothing }
                    Right aVal -> pure . Right $ aContent { rootDir = Just aVal }
                _ -> pure . Right $ aContent { rootDir = Just aVal }
          -}
          -- HERE: modify based on new parameters processing:
          pure $ Right aContent
    _ -> pure . Left $ "@[parseFileOptions] unknown conf-file extension: " <> fileExt
