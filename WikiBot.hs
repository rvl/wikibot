{-# LANGUAGE RecordWildCards, OverloadedStrings, LambdaCase #-}

module Main where

import Web.Slack hiding (getConfig)
import Control.Monad (forever)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid

import Config hiding (SlackConfig)
import Search (doSearch, WikiSearchResult(..), DocSearch, makeDocSearch)

main :: IO ()
main = do
  Right config@Config{..} <- getConfig "config.yml"
  let slackConfig = SlackConfig { _slackApiToken = cfgSlackAPIToken cfgSlack }
      docSearch = makeDocSearch config
  withSlackHandle slackConfig (wikiBot config docSearch)

wikiBot :: Config -> DocSearch -> SlackHandle -> IO ()
wikiBot cfg docSearch h = forever $ do
  event <- getNextEvent h
  case event of
    (Message cid _ msg _ _ _) -> do
      handleSearchMessage docSearch msg >>= \case
        Right results -> sendMessage h cid results
        Left _ -> return ()
    _ -> return ()

handleSearchMessage :: DocSearch -> Text -> IO (Either String Text)
handleSearchMessage docSearch q = fmap formatResults <$> doSearch docSearch q
  where
    formatResults :: [WikiSearchResult] -> Text
    formatResults [] = "wikibot finds nothing"
    formatResults rs = T.intercalate "\n" . map fmt $ rs
    fmt :: WikiSearchResult -> Text
    fmt (WikiSearchResult t u) = T.unlines [t, u]
