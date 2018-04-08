{-# LANGUAGE DeriveGeneric, OverloadedStrings, LambdaCase #-}

module Config
  ( Config(..)
  , SlackConfig(..)
  , ElasticConfig(..)
  , ServerConfig(..)
  , RespondTo(..)
  , RespondWhere(..)
  , getConfig
  , getConfigOrDie
  , getConfigFromEnv
  , customOptions
  ) where

import GHC.Generics
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Yaml
import Control.Error
import Network.URI (URI, parseAbsoluteURI)
import Data.Foldable (toList)
import System.Exit (exitFailure)
import System.Environment (lookupEnv)
import Say
import Formatting (sformat, string, (%))
import Data.Maybe (fromMaybe)

getConfig :: FilePath -> IO (Either String Config)
getConfig = fmap (fmapL show) . decodeFileEither

getConfigOrDie :: FilePath -> IO Config
getConfigOrDie f = getConfig f >>= either die pure
  where
    die e = do
      sayErr $ sformat (string%": Problem reading config file: "%string) f e
      exitFailure

getConfigFromEnv :: IO Config
getConfigFromEnv = do
  configFile <- fromMaybe "config.yml" <$> lookupEnv "WIKIBOT_CONFIG"
  getConfigOrDie configFile

data Config        = Config        { cfgDocsDir  :: FilePath
                                   , cfgBuildDir :: FilePath
                                   , cfgWikiURL  :: URI
                                   , cfgSlack    :: SlackConfig
                                   , cfgElastic  :: ElasticConfig
                                   , cfgServer   :: ServerConfig
                                   } deriving (Generic, Show, Eq)

data SlackConfig   = SlackConfig   { cfgSlackAPIToken :: String
                                   , cfgSlackBotToken :: String
                                   , cfgSlackVerificationToken :: Text
                                   , cfgSlackRespond :: RespondTo
                                   } deriving (Generic, Show, Eq)

data ElasticConfig = ElasticConfig { cfgElasticServer :: Text
                                   , cfgElasticIndexName :: Text
                                   } deriving (Generic, Show, Eq)

data ServerConfig = ServerConfig { cfgServerListenAddress :: String
                                 , cfgServerListenPort :: Int
                                 , cfgServerBaseURL :: URI
                                 } deriving (Generic, Show, Eq)

data RespondWhere = RespondToUser Text | RespondInChannel Text deriving (Show, Eq)
data RespondTo = RespondToAll | RespondTo [RespondWhere] deriving (Show, Eq)

instance FromJSON Config where
  parseJSON = genericParseJSON (customOptions' 1)

instance FromJSON SlackConfig where
  parseJSON = genericParseJSON (customOptions' 2)

instance FromJSON ElasticConfig where
  parseJSON = genericParseJSON (customOptions' 2)

instance FromJSON ServerConfig where
  parseJSON = genericParseJSON (customOptions' 2)

instance FromJSON RespondWhere where
  parseJSON = withText "Entity" $ \t -> case T.splitAt 1 t of
    ("@", u) -> pure $ RespondToUser u
    ("#", c) -> pure $ RespondInChannel c
    _ -> fail "Expected: #channel or @user"

instance FromJSON RespondTo where
  parseJSON (String "*") = pure RespondToAll
  parseJSON (Array a) = RespondTo <$> mapM parseJSON (toList a)
  parseJSON v = typeMismatch "\"*\" or list" v

-- instance FromJSON (URIRef Absolute)  where
--   parseJSON = withText "URI" parse
--     where parse u = case parseURI strictURIParserOptions (encodeUtf8 u) of
--                       Right uri -> pure uri
--                       Left _ -> fail "Could not parse absolute URI"

instance FromJSON (URI)  where
  parseJSON = withText "URI" parse
    where parse u = case parseAbsoluteURI (T.unpack u) of
                      Just uri -> pure uri
                      Nothing -> fail "Could not parse absolute URI"

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
