{-# LANGUAGE DeriveGeneric #-}

module Config
  ( Config(..)
  , SlackConfig(..)
  , ElasticConfig(..)
  , getConfig
  , customOptions
  ) where

import GHC.Generics
import Data.Text (Text)
import Data.Aeson
import Data.Yaml
import Control.Error

getConfig :: FilePath -> IO (Either String Config)
getConfig = fmap (fmapL show) . decodeFileEither

data Config        = Config        { cfgDocsDir  :: FilePath
                                   , cfgBuildDir :: FilePath
                                   , cfgWikiURL  :: Text
                                   , cfgSlack    :: SlackConfig
                                   , cfgElastic  :: ElasticConfig
                                   } deriving (Generic, Show, Eq)

data SlackConfig   = SlackConfig   { cfgSlackAPIToken :: String
                                   } deriving (Generic, Show, Eq)

data ElasticConfig = ElasticConfig { cfgElasticServer :: Text
                                   , cfgElasticIndexName :: Text
                                   } deriving (Generic, Show, Eq)

instance FromJSON Config where
  parseJSON = genericParseJSON (customOptions' 1)

instance FromJSON SlackConfig where
  parseJSON = genericParseJSON (customOptions' 2)

instance FromJSON ElasticConfig where
  parseJSON = genericParseJSON (customOptions' 2)

customOptions = customOptions' 1

customOptions' l = defaultOptions { fieldLabelModifier = nTimes l dropPrefix . camelTo2 '_' }
  where
    dropPrefix :: String -> String
    dropPrefix = drop 1 . dropWhile (/= '_')

-- | Compose a function with itself n times.  (nth rather than twice)
nTimes :: Int -> (a -> a) -> (a -> a)
nTimes 0 _ = id
nTimes 1 f = f
nTimes n f = f . nTimes (n-1) f
