{-# LANGUAGE RecordWildCards, OverloadedStrings, LambdaCase #-}

module Main where

import Web.Slack hiding (getConfig)
import qualified Web.Slack as Slack
import Web.Slack.WebAPI (getUser, getChannel)
import qualified Web.Slack.Types.Format as Slack
import Control.Monad (forever, unless, void, guard)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT, exceptToMaybeT)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Char8 as S8
import Data.Monoid
import Formatting (sformat, stext, shown, int, string, (%))
import Data.Colour (blend)
import Data.Colour.SRGB (sRGB24show)
import qualified Data.Colour.Names as C
import Network.URI (URI(..), relativeTo, parseRelativeReference)
import Network.HTTP.Types.URI (renderSimpleQuery)
import Data.Maybe (fromJust, fromMaybe, catMaybes, isJust)
import Control.Concurrent.Async
import Safe (readMay, headMay)
import Say
import Network.Wai.Middleware.RequestLogger (logStdout)
import qualified Network.Wai.Handler.Warp as Warp
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.LocalTime (zonedTimeToUTC)
import Control.Monad.Catch (handle)
import Options.Applicative

import Config hiding (SlackConfig)
import Search (doSearch, WikiSearchResult(..), Document(..), DocSearch, makeDocSearch, Paging(..), SearchError(..))

main :: IO ()
main = wikiBotWithConfig =<< execParser opts
  where
    opts = info (cfg <**> helper)
      ( fullDesc
        <> progDesc "Runs the Slack bot."
        <> header "wikibot - Your GitHub Wiki is now a Slack bot" )
    cfg = strOption
          ( long "config" <> short 'c' <> metavar "FILE"
            <> help "YAML file with config"  )

wikiBotWithConfig :: FilePath -> IO ()
wikiBotWithConfig configFile = do
  config <- getConfigOrDie configFile
  wikiBot config (makeDocSearch config)

-- | Connect to Slack RTM then run both the RTM handlers and events
-- API handlers.
wikiBot :: Config -> DocSearch -> IO ()
wikiBot config docSearch = void $ withSlackHandle (slackConfig config) $ \h ->
  concurrently (rtmMain config docSearch h) (apiMain config docSearch h)

-- | Server to receive HTTP callbacks from slack
apiMain :: Config -> DocSearch -> SlackHandle -> IO ()
apiMain cfg@Config{..} docSearch h = do
  app <- eventsApplication appSettings
  say $ sformat ("Events API server listening on port " % int) (cfgServerListenPort cfgServer)
  Warp.run (cfgServerListenPort cfgServer) (middleware app)
  where
    middleware = logStdout
    appSettings = EventsApplication (cfgSlackVerificationToken cfgSlack)
      [("/wikisearch", slashCommand cfg docSearch h)]
      (handleInteraction cfg docSearch h)
      (const (pure Nothing))

-- | Responds to /wikisearch requests.
slashCommand :: Config -> DocSearch -> SlackHandle -> CommandRequest -> IO (Maybe EventResponse)
slashCommand cfg docSearch h CommandRequest{..} = Just <$> (decide >>= respond)
  where
    respond True  = handleSearchMessage cfg Nothing docSearch slackCommandRequestText 0
    respond False = pure $ EventResponse "Wikibot is not allowed to talk to you" []
    decide = decideToRespondEvents cfg h slackCommandRequestChannelId slackCommandRequestUserId

-- | Responds to interaction events (more/less button presses).
handleInteraction :: Config -> DocSearch -> SlackHandle -> Interaction -> IO (Maybe EventResponse)
handleInteraction cfg docSearch h Interaction{..} =
  decide >>= \t -> if t then Just <$> respond else pure Nothing
  where
    respond = case catMaybes (map newPage interactionActions) of
      (page:_) -> do
        say $ sformat (stext % " wants " % stext) (userInfoUserName interactionUser) (mconcat $ map actionName interactionActions)
        handleSearchMessage cfg Nothing docSearch interactionCallbackId page
      []       -> pure $ EventResponse "Wikibot is confused." []

    newPage a = pageNum a >>= pageMod a
      where
        pageNum :: Action -> Maybe Int
        pageNum = (>>= readMay) . fmap T.unpack . actionValue
        pageMod :: Action -> Int -> Maybe Int
        pageMod a p = case actionName a of
          "more" -> Just (p + 1)
          "less" -> Just (p - 1)
          _      -> Nothing

    -- fixme: is channel id returned in message?
    decide = decideToRespondEvents cfg h (Id "") (userInfoUserId interactionUser)

slackConfig :: Config -> SlackConfig
slackConfig Config{..} = SlackConfig { _slackApiToken = cfgSlackBotToken cfgSlack }

-- | RTM is the websocket API for slack
rtmMain :: Config -> DocSearch -> SlackHandle -> IO ()
rtmMain cfg docSearch h = forever $ do
  event <- getNextEvent h
  case event of
    Hello -> say "Connected to slack."
    (Message cid (UserComment uid) msg ts subtype _) -> do
      let msg' = Slack.format msg
      decideToRespondRTM cfg h cid uid subtype msg' >>= \case
        Just query -> do
          EventResponse txt ats <- handleSearchMessage cfg (Just uid) docSearch query 0
          void $ sendRichMessage h cid txt ats
        Nothing -> pure ()
    (HiddenMessage cid _ ts (Just (SMessageChanged upd))) -> do
      -- TODO: Message edits could be used to refine the
      -- search. However this would require holding on to message timestamps
      -- of previous searches and their results.
      pure ()
    _ -> pure ()

-- | Decide whether to respond to a channel message or DM.
decideToRespondRTM :: Config -> SlackHandle
                   -> ChannelId -> UserId -> Maybe Subtype
                   -> [Slack.Format] -> IO (Maybe Text)
decideToRespondRTM = decideToRespond False

-- | Decide whether to respond to a slash command or interaction button press.
decideToRespondEvents :: Config -> SlackHandle -> ChannelId -> UserId -> IO Bool
decideToRespondEvents cfg h cid uid = isJust <$> decideToRespond True cfg h cid uid Nothing []

-- | Decide whether to respond to a chat message or DM.
-- Returns the message with @mention text removed if the bot should
-- respond.
decideToRespond :: Bool -> Config -> SlackHandle
                -> ChannelId -> UserId -> Maybe Subtype
                -> [Slack.Format] -> IO (Maybe Text)
decideToRespond isEvent cfg h cid uid t msg = runMaybeT $
  decideToRespondBase isEvent (Slack.getConfig h) rto self cids cid uid t msg
  where
    self = _selfUserId . _slackSelf . getSession $ h
    cids = canRespondToChannelIds h rto
    rto  = cfgSlackRespond $ cfgSlack cfg

-- | This method controls how freely wikibot shares search
-- results. Usually a bot will join any channel it is invited to, and
-- respond to direct messages (DMs).
--
-- We need to close this down, so that wikibot will only reply in
-- whitelisted channels or to whitelisted users. If wikibot is sent a
-- DM, only reply if the sender is a member of a whitelisted channel.
--
-- If bot should respond, returns message without @mention.
decideToRespondBase :: Bool -> SlackConfig -> RespondTo
                    -> UserId -> [ChannelId]
                    -> ChannelId -> UserId -> Maybe Subtype
                    -> [Slack.Format] -> MaybeT IO Text
decideToRespondBase isEvent cfg rto self cids cid uid t msg = do
  -- don't respond to self and only normal chat messages
  guard (uid /= self && goodSubtype t)
  -- respond to DMs or @mentions in whitelisted channels
  guard (isEvent || isDM cid || (Slack.isMentioned self msg && cid `elem` cids))
  -- check if username is whitelisted
  user <- exceptToMaybeT . fmap _userName $ getUser cfg uid
  unless (canRespondToUsername rto user) $ do
    -- failing that, check if user is in a whitelisted channel
    people <- allChannelMembers cfg cids
    guard (uid `elem` people)
  pure $ Slack.unformatOnlyText msg
  where
    goodSubtype :: Maybe Subtype -> Bool
    goodSubtype Nothing = True
    goodSubtype (Just SMeMessage) = True -- fixme: what is memessage
    goodSubtype (Just (SMessageChanged _)) = True
    goodSubtype _ = False

-- | Finds the members of all the channels that wikibot may respond in.
allChannelMembers :: SlackConfig -> [ChannelId] -> MaybeT IO [UserId]
allChannelMembers cfg cids = exceptToMaybeT . concatMapM getChannelMembers $ cids
  where
    getChannelMembers = fmap members . getChannel cfg
    members = fromMaybe [] . _channelMembers
    concatMapM f = fmap concat . mapM f

-- | The channel ids of whitelisted channels that the bot is a member of.
canRespondToChannelIds :: SlackHandle -> RespondTo -> [ChannelId]
canRespondToChannelIds h = concatMap findId . canRespondInChannels
  where
    -- Find ChannelId of a channel that the bot is in.
    findId :: Text -> [ChannelId]
    findId name = [_channelId c | c <- _slackChannels (getSession h), _channelName c == name]

-- | Direct message channels are prefixed with D instead of C.
isDM :: ChannelId -> Bool
isDM (Id cid) = T.take 1 cid == "D"

-- | Whether wikibot may respond to this username
canRespondToUsername :: RespondTo -> Text -> Bool
canRespondToUsername RespondToAll _ = True
canRespondToUsername (RespondTo w) u = or [u == u' | RespondToUser u' <- w]

-- | Names of channels that wikibot may respond to members of.
canRespondInChannels :: RespondTo -> [Text]
canRespondInChannels RespondToAll = []
canRespondInChannels (RespondTo w) = [c | RespondInChannel c <- w]

-- | Prefixes message text with @user
mentionUser :: Maybe UserId -> EventResponse -> EventResponse
mentionUser (Just (Id u)) (EventResponse txt as) = EventResponse (sformat ("<@"%stext%"> "%stext) u txt) as
mentionUser Nothing r = r

handleSearchMessage :: Config -> Maybe UserId -> DocSearch -> Text -> Int -> IO EventResponse
handleSearchMessage cfg uid docSearch q page = do
  say $ sformat ("Search from " % stext % " for: " % stext) (maybe "-" _getId uid) q
  handle errMsg (mentionUser uid . uncurry formatResults <$> res)
  where
    res = doSearch docSearch (Paging 0 count) q
    count = (page + 1) * 5

    formatResults :: Int -> [WikiSearchResult] -> EventResponse
    formatResults _ [] = emptyMsg
    formatResults total rs = EventResponse
      (sformat (int % " results for " % stext) total (quoteSearch q))
      (map fmt rs ++ if length rs < total then [more total] else [])


    fmt :: WikiSearchResult -> Attachment
    fmt WikiSearchResult{..} = defaultAttachment
      { attachmentFallback = uriText wikiResURL
      , attachmentTitle = Just (docTitle wikiResDoc)
      , attachmentTitleLink = Just $ uriText wikiResURL
      , attachmentText = Nothing -- wikiResHighlight / summary?
      , attachmentAuthorName = Nothing -- last edited by? is quite prominent
      , attachmentAuthorLink = Nothing -- last edited by?
      , attachmentFooter = Just $ sformat ("updated by " % stext) (docUpdatedBy wikiResDoc) -- score, document info
      , attachmentColor = maybe DefaultColor (CustomColor . scoreColour) wikiResScore
      , attachmentTs = Just . utcTimeToPOSIXSeconds . zonedTimeToUTC . docUpdatedOn $ wikiResDoc  -- last modified time
      }

    more total = defaultAttachment
      { attachmentFallback = desc
      , attachmentText = Just desc
      , attachmentActions = (if page > 0 then [action "less" "Less"] else []) ++ [action "more" "More..."]
      , attachmentCallbackId = Just q
      }
      where
        desc = sformat ("Showing " % int % " of " % int % " results") count total
        action name t = Action
          { actionName = name
          , actionText = t
          , actionType = ButtonType
          , actionValue = Just (sformat int page)
          , actionStyle = Just DefaultStyle
          , actionOptions = []
          }

    emptyMsg = EventResponse ("Wikibot finds nothing about " <> quoteSearch q) [newPageAttachment]
      where
        newPageAttachment = defaultAttachment
          { attachmentFallback = sformat ("Create new page: " % shown) newUrl
          , attachmentTitle = Just "Create new page"
          , attachmentTitleLink = Just $ uriText newUrl'
          , attachmentText = Just "Start writing a new document."
          }
        newUrl = relURI "_new" `relativeTo` cfgWikiURL cfg
        newUrl' = newUrl { uriQuery = queryString [("wiki[body]", q)] }

    errMsg :: SearchError -> IO EventResponse
    errMsg e = do
      say $ sformat ("Error searching: " % shown) e
      pure $ EventResponse "Wikibot can't think at the moment" []

relURI :: Text -> URI
relURI = fromJust . parseRelativeReference . T.unpack

uriText :: URI -> Text
uriText = T.pack . show

queryString :: [(Text, Text)] -> String
queryString q = S8.unpack (renderSimpleQuery True [(encodeUtf8 k, encodeUtf8 v) | (k, v) <- q])

-- | Show query string with any "user-supplied" formatting
quoteSearch :: Text -> Text
quoteSearch q = "_" <> safe q <> "_"
  where
    safe = strip '\n' . strip '_' . strip '*' . strip '`'
    strip c = T.replace (T.pack [c]) ""

scoreColour :: Double -> Text
scoreColour c = T.pack . sRGB24show $ blend c' C.royalblue C.whitesmoke
  where c' = min (c / 10.0) 1.0
