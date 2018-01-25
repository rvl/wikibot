{-# LANGUAGE DeriveGeneric, PackageImports, RecordWildCards, OverloadedStrings, Rank2Types #-}

module Search
  ( Document(..)
  , makeDocument
  , WikiSearchResult(..)
  , doSearch
  , DocSearch(..)
  , makeDocSearch
  , deleteDocIndex
  , createDocIndex
  , indexSearchDoc
  ) where

import Database.V5.Bloodhound
import Network.HTTP.Client
import GHC.Generics
import Data.Text (Text)
import qualified Data.Text as T
import "cryptonite" Crypto.Hash
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as LS8
import Data.Aeson
import Development.Shake.FilePath
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString (ByteString)
import Control.Monad (void, when)
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import System.FilePath.Posix (dropExtension)
import qualified Data.Map.Lazy as M

import Config

data Document = Document { docId :: Text
                         , docContent :: Text
                         , docFileName :: FilePath
                         , docTitle  :: Text
                         } deriving (Eq, Generic, Show)

instance ToJSON Document where
  toJSON = genericToJSON customOptions

instance FromJSON Document where
  parseJSON = genericParseJSON customOptions

makeDocument :: FilePath -> FilePath -> String -> Document
makeDocument base doc html = Document (T.pack id) (T.pack html) name (T.pack "")
  where
    id = makeDocumentId name
    name = makeRelative base doc

makeDocumentId :: FilePath -> String
makeDocumentId md = show (hash bs :: Digest SHA1)
  where bs = S8.pack md

data DocSearch = DocSearch
  { docSearchRun :: forall a. BH IO a -> IO a
  , docSearchIdx :: IndexName
  , docSearchMapping :: MappingName
  , docSearchConfig :: Config
  }

makeDocSearch :: Config -> DocSearch
makeDocSearch cfg@Config{..} = DocSearch
  { docSearchRun = withBH defaultManagerSettings testServer
  , docSearchIdx = IndexName $ cfgElasticIndexName cfgElastic
  , docSearchMapping = MappingName "doc"
  , docSearchConfig = cfg
  }
  where
    testServer = Server (cfgElasticServer cfgElastic)

createDocIndex :: DocSearch -> IO (Response LS8.ByteString)
createDocIndex DocSearch{..} = docSearchRun $ do
  exists <- indexExists docSearchIdx
  when exists . void $ deleteIndex docSearchIdx
  createIndex defaultIndexSettings docSearchIdx

deleteDocIndex :: DocSearch -> IO ()
deleteDocIndex DocSearch{..} = void . docSearchRun $ deleteIndex docSearchIdx

indexSearchDoc :: DocSearch -> Document -> IO (Response LS8.ByteString)
indexSearchDoc DocSearch{..} doc = docSearchRun $
  indexDocument docSearchIdx docSearchMapping defaultIndexDocumentSettings doc (DocId (docId doc))

data WikiSearchResult = WikiSearchResult
  { wikiResHighlight :: Text
  , wikiResURL :: Text
  } deriving (Generic, Show, Eq)

doSearch :: DocSearch -> Text -> IO (Either String [WikiSearchResult])
doSearch DocSearch{..} q = do
  putStrLn $ "search for " ++ (T.unpack q)
  reply <- docSearchRun $ searchAll search
  -- putStrLn $ "reply: " ++ (LS8.unpack (responseBody reply))
  let res = eitherDecode (responseBody reply) :: Either String (SearchResult Document)
      hs = hits . searchHits <$> res :: Either String [Hit Document]
  return . fmap (map (makeResult docSearchConfig)) $ hs
  where
    -- query = TermQuery (Term "content" q) Nothing
    -- fixme: need to search on content not _all in order to get highlights
    query = QueryMatchQuery $ mkMatchQuery (FieldName "content") (QueryString q)
    search = mkHighlightSearch (Just query) highlights
    highlights = Highlights Nothing [FieldHighlight (FieldName "content") Nothing]

makeResult :: Config -> Hit Document -> WikiSearchResult
makeResult Config{..} hit = WikiSearchResult (fromMaybe title hl) (fromMaybe docId url)
  where
    hl = T.unlines . map toMarkdown <$> (hitHighlight hit >>= M.lookup "content")
    docId = T.pack . show . hitDocId $ hit
    url = makeDocUrl cfgWikiURL <$> hitSource hit
    title = fromMaybe "No title" (docTitle <$> hitSource hit)

makeDocUrl :: Text -> Document -> Text
makeDocUrl baseUrl Document{..} = baseUrl <> T.pack basename
  where basename = dropExtension docFileName

toMarkdown :: Text -> Text
toMarkdown = id
