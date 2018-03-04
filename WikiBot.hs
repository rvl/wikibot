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
import Formatting (sformat, text, shown, float, int, (%))
import Data.Colour (blend)
import Data.Colour.SRGB (sRGB24show)
import qualified Data.Colour.Names as C
import Network.URI (URI(..), relativeTo, parseRelativeReference, parseAbsoluteURI)
import Network.HTTP.Types.URI (renderSimpleQuery)
import Data.Maybe (fromJust)
import Data.Aeson (Value(..), eitherDecode)
import Control.Concurrent.Async
import Say
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import qualified Network.Wai.Handler.Warp as Warp

import Config hiding (SlackConfig)
import Search (doSearch, WikiSearchResult(..), DocSearch, makeDocSearch)
import SlackEvents

main :: IO ()
main = do
  Right config@Config{..} <- getConfig "config.yml"
  let docSearch = makeDocSearch config

  void $ concurrently
    (rtmMain config docSearch)
    (apiMain config)

-- | RTM is the websocket API for slack
rtmMain :: Config -> DocSearch -> IO ()
rtmMain config docSearch = withSlackHandle (slackConfig config) $ wikiBot config docSearch

-- | Server to receive HTTP callbacks from slack
apiMain :: Config -> IO ()
apiMain Config{..} = do
  app <- slackEventsApplication cfg
  let middleware = logStdoutDev . dropBasePath (cfgServerBaseURL cfgServer)
  Warp.run (cfgServerListenPort cfgServer) (middleware app)
  where
    cfg = SlackEventsSettings (cfgSlackVerificationToken cfgSlack) [SlackCommandHandler "/wikisearch" cmd]
    cmd SlackCommandRequest{..} = return . Just $ "you said " <> slackCommandRequestText

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
        handleSearchMessage cfg uid docSearch msg >>= \case
          Right (txt, ats) -> sendRichMessage h cid txt ats >> return ()
          Left _ -> return ()
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

handleSearchMessage :: Config -> UserId -> DocSearch -> Text -> IO (Either String (Text, [Attachment]))
handleSearchMessage cfg _ docSearch q = fmap formatResults <$> doSearch docSearch q
  where
    formatResults :: [WikiSearchResult] -> (Text, [Attachment])
    formatResults [] = ("wikibot finds nothing about " <> quoteSearch q, [newPageAttachment])
      where
        newPageAttachment = defaultAttachment
          { attachmentFallback = sformat ("Create new page: " % shown) newUrl
          , attachmentTitle = Just "Create new page"
          , attachmentTitleLink = Just $ uriText newUrl'
          , attachmentText = Just "Start writing a new document."
          }
        newUrl = relURI "_new" `relativeTo` cfgWikiURL cfg
        newUrl' = newUrl { uriQuery = queryString [("wiki[body]", q)] }
    formatResults rs = ( "results for " <> quoteSearch q
                       , map fmt first ++ if null rest then [] else [more])
      where
        count = 5
        (first, rest) = splitAt count rs
        fmt :: WikiSearchResult -> Attachment
        fmt WikiSearchResult{..} = defaultAttachment
          { attachmentFallback = uriText wikiResURL
          , attachmentTitle = Just wikiResTitle
          , attachmentTitleLink = Just $ uriText wikiResURL
          , attachmentText = Nothing -- wikiResHighlight / summary?
          , attachmentAuthorName = Nothing -- last edited by? is quite prominent
          , attachmentAuthorLink = Nothing -- last edited by?
          , attachmentFooter = fmap (sformat ("score = " % float)) wikiResScore -- score, document info
          , attachmentColor = maybe DefaultColor (CustomColor . scoreColour) wikiResScore
          , attachmentTs = Nothing -- last modified time
          }
        moreDesc = sformat ("Showing " % int % " of " % int % " results") count (length rs)
        more = defaultAttachment
          { attachmentFallback = moreDesc
          , attachmentText = Just moreDesc
          , attachmentActions = [moreResultsAction]
          , attachmentCallbackId = Just "yo"
          }
        moreResultsAction = Action
          { actionName = "more"
          , actionText = "More..."
          , actionType = ButtonType
          , actionValue = Nothing
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
