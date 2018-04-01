{-# LANGUAGE RecordWildCards, OverloadedStrings, LambdaCase #-}

module Main where

import Web.Slack hiding (getConfig)
import Control.Monad (forever, when, void)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as BL
import Data.Monoid
import Formatting (format, sformat, text, stext, shown, float, int, string, (%))
import Data.Colour (blend)
import Data.Colour.SRGB (sRGB24show)
import qualified Data.Colour.Names as C
import Network.URI (URI(..), relativeTo, parseRelativeReference, parseAbsoluteURI)
import Network.HTTP.Types.URI (renderSimpleQuery)
import Data.Maybe (fromJust, catMaybes)
import Data.Aeson (Value(..), eitherDecode, toJSON)
import Control.Concurrent.Async
import Safe (readMay)
import Say
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import qualified Network.Wai.Handler.Warp as Warp
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.LocalTime (zonedTimeToUTC)
import Control.Monad.Catch (try)

import Config hiding (SlackConfig)
import Search (doSearch, WikiSearchResult(..), Document(..), DocSearch, makeDocSearch, Paging(..), SearchError(..))
import SlackEvents

main :: IO ()
main = do
  Right config@Config{..} <- getConfig "config.yml"
  let docSearch = makeDocSearch config

  void $ concurrently
    (rtmMain config docSearch)
    (apiMain config docSearch)

-- | RTM is the websocket API for slack
rtmMain :: Config -> DocSearch -> IO ()
rtmMain config docSearch = withSlackHandle (slackConfig config) $ wikiBot config docSearch

-- | Server to receive HTTP callbacks from slack
apiMain :: Config -> DocSearch -> IO ()
apiMain cfg@Config{..} docSearch = do
  app <- eventsApplication appSettings
  let middleware = logStdoutDev . dropBasePath (cfgServerBaseURL cfgServer)
  Warp.run (cfgServerListenPort cfgServer) (middleware app)
  where
    appSettings = EventsApplication (cfgSlackVerificationToken cfgSlack) [("/wikisearch", cmd)] actionHandler (const (pure Nothing))
    cmd CommandRequest{..} = handleSearchMessage cfg slackCommandRequestUserId docSearch slackCommandRequestText 0 >>= \case
      Right (txt, ats) -> pure . Just $ EventResponse txt ats
      Left _ -> pure . Just $ EventResponse "Sorry, that didn't work." []
    actionHandler Interaction{..} = do
      let UserInfo{..} = interactionUser
          q = interactionCallbackId
      say $ sformat (stext % " wants more") userInfoUserName
      case catMaybes (map ((>>= readMay) . fmap T.unpack . actionValue) interactionActions) of
        (count:_) -> handleSearchMessage cfg userInfoUserId docSearch q (count + 1) >>= \case
          Right (txt, ats) -> pure . Just $ EventResponse txt ats
          Left _ -> pure . Just $ EventResponse "Sorry, that didn't work." []
        otherwise -> pure . Just $ EventResponse "Wikibot is confused." []

slackConfig :: Config -> SlackConfig
slackConfig Config{..} = SlackConfig { _slackApiToken = cfgSlackBotToken cfgSlack }

wikiBot :: Config -> DocSearch -> SlackHandle -> IO ()
wikiBot cfg docSearch h = forever $ do
  event <- getNextEvent h
  case event of
    Hello -> say "Connected to slack."
    (Message cid (UserComment uid) msg ts subtype _) -> do
      putStrLn $ "ts is " ++ show ts
      shouldRespond <- decideToRespond cid uid subtype msg h
      when shouldRespond $ do
        handleSearchMessage cfg uid docSearch msg 0 >>= \case
          Right (txt, ats) -> void $ sendRichMessage h cid txt ats
          Left e -> do
            putStrLn $ "Problem searching: " ++ show e
            void $ sendRichMessage h cid "Wikibot can't think at the moment" []
    (HiddenMessage cid _ ts (Just (SMessageChanged upd))) -> do
      putStrLn $ "ts is " ++ show ts
      putStrLn $ "update is " ++ show upd
    msg -> print msg
    _ -> return ()

decideToRespond :: ChannelId -> UserId -> Maybe Subtype -> Text -> SlackHandle -> IO Bool
decideToRespond cid uid t msg h = pure (uid /= self && goodSubtype t)
  where
    self = _selfUserId . _slackSelf . getSession $ h

goodSubtype :: Maybe Subtype -> Bool
goodSubtype Nothing = True
goodSubtype (Just SMeMessage) = True -- fixme: what is memessage
goodSubtype (Just (SMessageChanged _)) = True
goodSubtype _ = False

handleSearchMessage :: Config -> UserId -> DocSearch -> Text -> Int -> IO (Either SearchError (Text, [Attachment]))
handleSearchMessage cfg _ docSearch q page =
  try (uncurry formatResults <$> doSearch docSearch (Paging 0 count) q)
  where
    count = (page + 1) * 5
    formatResults :: Int -> [WikiSearchResult] -> (Text, [Attachment])
    formatResults _ [] = ("wikibot finds nothing about " <> quoteSearch q, [newPageAttachment])
      where
        newPageAttachment = defaultAttachment
          { attachmentFallback = sformat ("Create new page: " % shown) newUrl
          , attachmentTitle = Just "Create new page"
          , attachmentTitleLink = Just $ uriText newUrl'
          , attachmentText = Just "Start writing a new document."
          }
        newUrl = relURI "_new" `relativeTo` cfgWikiURL cfg
        newUrl' = newUrl { uriQuery = queryString [("wiki[body]", q)] }
    formatResults total rs = (sformat (int % " results for " % stext) total (quoteSearch q)
                             , map fmt rs ++ if length rs < total then [more] else [])
      where
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
        moreDesc = sformat ("Showing " % int % " of " % int % " results") count total
        more = defaultAttachment
          { attachmentFallback = moreDesc
          , attachmentText = Just moreDesc
          , attachmentActions = [moreResultsAction]
          , attachmentCallbackId = Just q
          }
        moreResultsAction = Action
          { actionName = "more"
          , actionText = "More..."
          , actionType = ButtonType
          , actionValue = Just (sformat int page)
          , actionStyle = Just DefaultStyle
          , actionOptions = []
          }

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
