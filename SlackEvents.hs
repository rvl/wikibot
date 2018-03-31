{-# LANGUAGE RecordWildCards, OverloadedStrings, LambdaCase, ScopedTypeVariables, GADTs, DeriveGeneric, Rank2Types, StandaloneDeriving #-}

module SlackEvents
  ( slackEventsApplication
  , SlackEventsSettings(..)
  , SlackCommandHandler(..)
  , SlackCommandRequest(..)
  , SlackResponse(..)
  , dropBasePath
  , UserInfo(..)
  , EventMessage(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.List (isPrefixOf, find)
import Network.Wai (Application, Middleware, Request(pathInfo))
import Network.HTTP.Types.Status (unauthorized401, notFound404, badRequest400)
import Web.Scotty
import Data.Aeson hiding (json)
import Data.Aeson.Types (Parser, parseEither)
import Network.URI
import Control.Error (fmapL, note)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Time.Clock.POSIX (POSIXTime)
import Safe (readMay)
import Control.Applicative ((<|>))
import Data.Monoid

import Web.Slack.Types.Id
import Web.Slack.Types.Message

data SlackEventsSettings = SlackEventsSettings
  { slackEventsVerificationToken :: Text
  , slackEventsCommandHandlers :: [SlackCommandHandler]
  , slackEventsActionHandler :: SlackActionHandler
  }

data SlackEvent where
  SlackEventChallenge :: Text -> SlackEvent
  SlackInteractiveMessage :: URI -> [Action] -> Maybe EventMessage -> CallbackId -> TriggerId -> UserInfo -> SlackEvent
  -- there is other stuff within interactive_event like ChannelId -> UserId -> Bool -> POSIXTime -> POSIXTime
  SlackEventUnknown :: String -> Value -> SlackEvent
  deriving (Show)

type SlackActionHandler = URI -> [Action] -> Maybe EventMessage -> CallbackId -> TriggerId -> UserInfo -> IO (Maybe SlackResponse)
type TriggerId = Text
type CallbackId = Text

data UserInfo = UserInfo
  { userInfoUserId :: UserId
  , userInfoUserName :: Text
  , userInfoTeamId :: TeamId
  , userInfoTeamDomain :: Text
  } deriving (Show, Eq)

data EventMessage = EventMessage
  { eventMessageTimestamp :: POSIXTime
  , eventMessageAttachments :: [Attachment]
  , eventMessageText :: Text
  , eventMessageUser :: UserId
  , eventMessageType :: Text
  , eventMessageBotId :: BotId
  } deriving Show

{-
data EventAction = EventAction
  { eventActionName :: Text
  , eventActionType :: Text
  , eventActionValue :: ActionType
  } deriving (Show, Eq)

instance FromJSON EventAction where
  parseJSON = withObject "EventAction" $ \o ->
    EventAction <$> o .: "name" <*> o .: "value" <*> o.: "type"
-}

data SlackCommandHandler = SlackCommandHandler
  { slackCommandName :: Text
  , slackCommandAction :: SlackCommandRequest -> IO (Maybe SlackResponse)
  }

data SlackCommandRequest = SlackCommandRequest
  { slackCommandRequestTeamId :: TeamId
  , slackCommandRequestChannelId :: ChannelId
  , slackCommandRequestUserId :: UserId
  , slackCommandRequestText :: Text
  , slackCommandRequestResponseURL :: URI
  , slackCommandRequestTriggerId :: Text
  } deriving (Show, Eq)

data SlackResponse = SlackResponse
  { slackResponseText :: Text
  , slackResponseAttachments :: [Attachment]
  } deriving (Show, Eq)

instance FromJSON SlackEvent where
  parseJSON = withObject "Event" $ \v -> do
    typ <- v .: "type"
    case typ of
      "url_verification" -> SlackEventChallenge <$> v .: "challenge"
      "interactive_message" -> SlackInteractiveMessage
        <$> v .: "response_url"
        <*> v .: "actions"
        <*> v .:? "original_message"
        <*> v .: "callback_id"
        <*> v .: "trigger_id"
        <*> parseUserInfo v
      t -> pure $ SlackEventUnknown t (Object v)
      _ -> fail $ "Unknown type: " ++ typ

parseUserInfo :: Object -> Parser UserInfo
parseUserInfo o = do
  u <- o .: "user"
  t <- o .: "team"
  UserInfo <$> u .: "id" <*> u .: "name" <*> t .: "id" <*> t .: "domain"

paramVerified :: FromJSON a => Text -> TL.Text -> ActionM a
paramVerified token name = param name >>= jsonDataWithToken' token

slackEventsApplication :: SlackEventsSettings -> IO Application
slackEventsApplication SlackEventsSettings{..} = scottyApp $ do
  post "/slack/action" $ do
    paramVerified slackEventsVerificationToken "payload" >>= \case
      SlackInteractiveMessage uri as msg cb tr u -> do
        (liftAndCatchIO $ slackEventsActionHandler uri as msg cb tr u) >>= \case
          Just response -> json response
          Nothing -> pure ()
      _ -> do
        status badRequest400
        text "Action is of wrong type"

  post "/slack/event" $ do
    jsonDataWithToken slackEventsVerificationToken >>= \case
      SlackEventChallenge c -> json $ object ["challenge" .= c]
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
            Just msg -> json msg
            Nothing -> Web.Scotty.text ""
          Nothing -> do
            status notFound404
            text "Unknown command"
      else badToken

  notFound $ do
    liftAndCatchIO $ putStrLn "scotty not found"
    json ("nope" :: Text)

tokenParser :: Text -> Value -> Parser ()
tokenParser token = withObject "Authenticated Request" $ \o -> do
  token' <- o .: "token"
  if token' == token
    then pure ()
    else fail "Verification token did not match"

jsonDataWithToken :: FromJSON a => Text -> ActionM a
jsonDataWithToken token = do
  v <- jsonData
  jsonDataWithToken' token v

jsonDataWithToken' :: FromJSON a => Text -> Value -> ActionM a
jsonDataWithToken' token v = case parseEither (tokenParser token) v of
    Right () -> case fromJSON v of
      Success a -> pure a
      Error e -> do
        status badRequest400
        liftAndCatchIO $ L8.putStrLn $ "v is " <> encode v
        Web.Scotty.text $ TL.pack $ "Problem decoding json: " ++ e ++ ". Value: " ++ show v
        finish
    Left _ -> badToken

badToken :: ActionM a
badToken = do
  status unauthorized401
  Web.Scotty.text ("Verification token did not match")
  finish

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
  parseParam = note "Could not parse URI" . parseAbsoluteURI . TL.unpack

instance Parsable (Id a) where
  parseParam = fmap Id . parseParam

instance FromJSON URI where
  parseJSON = withText "Absolute URI" $ \t ->
    case parseAbsoluteURI (T.unpack t) of
      Just u -> pure u
      Nothing -> error "Could not parse absolute URI reference"

instance FromJSON EventMessage where
  parseJSON = withObject "EventMessage" $ \o ->
    EventMessage <$> ts o <*> o .: "attachments" <*> o .: "text" <*> o .: "user" <*> o .: "type" <*> o .: "bot_id"
    where
      ts o = (o .: "ts") <|> ((o .: "ts") >>= str)
      str = withText "timestamp string" $ \s ->
        case (readMay (T.unpack s) :: Maybe Double) of
          Just ts -> pure (realToFrac ts)
          Nothing -> error "couldn't parse timestamp string as double"

instance ToJSON EventMessage where
  toJSON (EventMessage ts as text user t bot) = object [ "ts" .= ts, "attachments" .= as, "text" .= text, "user" .= user, "type" .= t, "bot_id" .= bot ]

instance ToJSON SlackResponse where
  toJSON (SlackResponse t as) = object [ "text" .= t, "attachments" .= as ]
