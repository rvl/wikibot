{-# LANGUAGE RecordWildCards, OverloadedStrings, LambdaCase, ScopedTypeVariables, GADTs, DeriveGeneric #-}

module SlackEvents
  ( slackEventsApplication
  , SlackEventsSettings(..)
  , SlackCommandHandler(..)
  , SlackCommandRequest(..)
  , dropBasePath
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.List (isPrefixOf, find)
import Network.Wai (Application, Middleware, Request(pathInfo))
import Network.HTTP.Types.Status (unauthorized401, notFound404)
import Web.Scotty
import Data.Aeson hiding (json)
import Network.URI
import Control.Error (fmapL, note)

data SlackEventsSettings = SlackEventsSettings
  { slackEventsVerificationToken :: Text
  , slackEventsCommandHandlers :: [SlackCommandHandler]
  }

data SlackEvent where
  SlackEventChallenge :: Text -> Text -> SlackEvent
  SlackEventUnknown :: String -> Value -> SlackEvent
  deriving (Show, Eq)

data SlackCommandHandler = SlackCommandHandler
  { slackCommandName :: Text
  , slackCommandAction :: SlackCommandRequest -> IO (Maybe Text)
  }

data SlackCommandRequest = SlackCommandRequest
  { slackCommandRequestTeamId :: Text
  , slackCommandRequestChannelId :: Text
  , slackCommandRequestUserId :: Text
  , slackCommandRequestText :: Text
  , slackCommandRequestResponseURL :: URI
  , slackCommandRequestTriggerId :: Text
  } deriving (Show, Eq)

instance FromJSON SlackEvent where
  parseJSON = withObject "Event" $ \v -> do
    typ <- v .: "type"
    case typ of
      "url_verification" -> SlackEventChallenge <$> v .: "token" <*> v .: "challenge"
      t -> pure $ SlackEventUnknown t (Object v)
      _ -> error $ "Unknown type: " ++ typ

slackEventsApplication :: SlackEventsSettings -> IO Application
slackEventsApplication SlackEventsSettings{..} = scottyApp $ do
  post "/slack/action" $ do
    payload <- param "payload" :: ActionM Value
    liftAndCatchIO . putStrLn $ "action endpoint " ++ show payload
    json $ Null
  post "/slack/event" $ do
    jsonData >>= \case
      SlackEventChallenge token c -> if token == slackEventsVerificationToken
        then json $ object ["challenge" .= c]
        else badToken
      _ -> json $ Null
  post "/slack/command" $ do
    token <- param "token"
    if token == slackEventsVerificationToken
      then do
        command <- param "command"
        req <- SlackCommandRequest
               <$> param "team_id"
               <*> param "channel_id"
               <*> param "user_id"
               <*> param "text"
               <*> param "response_url"
               <*> param "trigger_id"
        liftAndCatchIO . putStrLn $ "slash command " ++ show command ++ " " ++ show req
        case find ((== command) . slackCommandName) slackEventsCommandHandlers of
          Just handler -> liftAndCatchIO ((slackCommandAction handler) req) >>= \case
            Just msg -> json $ object ["text" .= msg] -- fixme: formatting types
            Nothing -> Web.Scotty.text ""
          Nothing -> do
            status notFound404
            text "Unknown command"
      else badToken

  notFound $ do
    liftAndCatchIO $ putStrLn "scotty not found"
    json ("nope" :: Text)

badToken :: ActionM ()
badToken = do
  status unauthorized401
  Web.Scotty.text ("Verification token did not match")

dropBasePath :: URI -> Middleware
dropBasePath base app req = app req'
  where
    req' | base' `isPrefixOf` parts = req { pathInfo = drop (length base') parts }
         | otherwise                = req
    parts = pathInfo req
    base' = filter (not . T.null) $ T.splitOn "/" (T.pack $ uriPath base)

instance Parsable Value where
  parseParam = fmapL TL.pack . eitherDecode . TL.encodeUtf8

instance Parsable URI where
  parseParam = note "Could not pase URI" . parseAbsoluteURI . TL.unpack
