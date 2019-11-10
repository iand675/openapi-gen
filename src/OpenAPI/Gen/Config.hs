module OpenAPI.Gen.Config where

import Control.Monad.Trans
import Data.Text (Text)
import Data.Aeson
import Data.Maybe (catMaybes)
import Data.Yaml

loadConfig :: MonadIO m => FilePath -> m Config
loadConfig = decodeFileThrow

data Config = Config
  { configSpec :: FilePath
  , configHaskell :: Maybe HaskellConfig
  } deriving (Show, Eq)

instance ToJSON Config where
  toJSON Config{..} = object
    [ "spec" .= configSpec
    , "haskell" .= configHaskell
    ]

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> Config
    <$> o .: "spec"
    <*> o .: "haskell"

data HaskellConfig = HaskellConfig
  { haskellConfigPackageName :: Text
  , haskellConfigRootModule :: Text
  , haskellConfigOutputPath :: Maybe FilePath
  } deriving (Show, Eq)

instance ToJSON HaskellConfig where
  toJSON HaskellConfig{..} = object (reqFields <> optFields)
    where
      reqFields =
        [ "packageName" .= haskellConfigPackageName
        , "rootModule" .= haskellConfigRootModule
        ]
      optFields = catMaybes
        [ ("outputPath" .=) <$> haskellConfigOutputPath
        ]

instance FromJSON HaskellConfig where
  parseJSON = withObject "HaskellConfig" $ \o ->
    HaskellConfig <$> o .: "packageName" <*> o .: "rootModule" <*> o .:? "outputPath"
