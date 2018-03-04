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
import Crypto.Hash
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as LS8
import Data.Aeson
import Development.Shake.FilePath
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString (ByteString)
import Control.Monad (void, when)
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import System.FilePath.Posix (dropExtension, takeBaseName)
import qualified Data.Map.Lazy as M
import Network.URI (URI, relativeTo, parseRelativeReference)

import Config

data Document = Document { docId :: Text
                         , docContent :: Text
                         , docHTML :: Text
                         , docFileName :: FilePath
                         , docTitle  :: Text
                         } deriving (Eq, Generic, Show)

instance ToJSON Document where
  toJSON = genericToJSON customOptions

instance FromJSON Document where
  parseJSON = genericParseJSON customOptions

data DocumentMapping = DocumentMapping deriving (Eq, Show)

instance ToJSON DocumentMapping where
  toJSON DocumentMapping =
    object
      [ "_all" .= object [ "enabled" .= False ]
      , "properties" .=
        object [ "title"   .= object [ "type" .= ("string" :: Text)
                                     , "analyzer" .= ("title_analyzer" :: Text) ]
               , "content" .= object [ "type" .= ("text" :: Text)
                                     , "analyzer" .= ("html_analyzer" :: Text) ]
               , "updated" .= object [ "type" .= ("date" :: Text)
                                     , "format" .= ("strict_date_optional_time||epoch_millis" :: Text ) ]
               ]
      ]

makeDocument :: FilePath -> FilePath -> String -> String -> Document
makeDocument base doc txt html = Document
  { docId = T.pack $ makeDocumentId name
  , docContent = T.pack txt
  , docHTML = T.pack html
  , docFileName = name
  , docTitle = titleFromFilename doc
  }
  where name = makeRelative base doc

makeDocumentId :: FilePath -> String
makeDocumentId md = show (hash bs :: Digest SHA1)
  where bs = S8.pack md

titleFromFilename :: FilePath -> Text
titleFromFilename = subs . T.pack . takeBaseName
  where
    subs = T.replace "-" " "

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
  , docSearchMapping = MappingName "wikidoc"
  , docSearchConfig = cfg
  }
  where
    testServer = Server (cfgElasticServer cfgElastic)

createDocIndex :: DocSearch -> IO (Response LS8.ByteString)
createDocIndex DocSearch{..} = docSearchRun $ do
  exists <- indexExists docSearchIdx
  when exists . void $ deleteIndex docSearchIdx

  let
    replicaCount = 0
    numShards = 1
    settings = [ NumberOfReplicas (ReplicaCount replicaCount)
               , AnalysisSetting wikiAnalysis ]
  res <- createIndexWith settings numShards docSearchIdx
  void $ putMapping docSearchIdx docSearchMapping DocumentMapping
  return res

wikiAnalysis :: Analysis
wikiAnalysis = Analysis
  { analysisAnalyzer    = M.fromList [("html_analyzer", ba), ("title_analyzer", ta)]
  , analysisTokenizer   = M.empty
  , analysisTokenFilter = M.fromList [("english_stemmer", stemmer), ("english_stop", stop)]
  , analysisCharFilter  = M.empty
  }
  where
    -- https://www.elastic.co/guide/en/elasticsearch/reference/current/analysis-htmlstrip-charfilter.html
    ba = AnalyzerDefinition
      { analyzerDefinitionTokenizer = Just (Tokenizer "standard")
      , analyzerDefinitionFilter = map TokenFilter ["standard", "lowercase", "english_stemmer", "english_stop"]
      , analyzerDefinitionCharFilter = [CharFilter "html_strip"]
      }
    ta = AnalyzerDefinition
      { analyzerDefinitionTokenizer = Just (Tokenizer "standard")
      , analyzerDefinitionFilter = map TokenFilter ["standard", "lowercase", "english_stemmer"]
      , analyzerDefinitionCharFilter = []
      }
    stemmer = TokenFilterDefinitionStemmer English
    stop = TokenFilterDefinitionStop (Left English)

deleteDocIndex :: DocSearch -> IO ()
deleteDocIndex DocSearch{..} = void . docSearchRun $ deleteIndex docSearchIdx

indexSearchDoc :: DocSearch -> Document -> IO (Response LS8.ByteString)
indexSearchDoc DocSearch{..} doc = docSearchRun $
  indexDocument docSearchIdx docSearchMapping defaultIndexDocumentSettings doc (DocId (docId doc))

data WikiSearchResult = WikiSearchResult
  { wikiResTitle :: Text
  , wikiResURL :: URI
  , wikiResHighlight :: Maybe Text
  , wikiResScore :: Maybe Double
  } deriving (Generic, Show, Eq)

doSearch :: DocSearch -> Text -> IO (Either String [WikiSearchResult])
doSearch DocSearch{..} q = do
  putStrLn $ "search for " ++ (T.unpack q)
  reply <- docSearchRun $ searchAll search
  -- putStrLn $ "reply: " ++ (LS8.unpack (responseBody reply))
  let res = eitherDecode (responseBody reply) :: Either String (SearchResult Document)
      hs = searchHits <$> res :: Either String (SearchHits Document)
  return (makeResults <$> hs)
  where
    makeResults :: SearchHits Document -> [WikiSearchResult]
    makeResults SearchHits{..} = map (makeResult docSearchConfig) hits
    query = QueryMultiMatchQuery $ mkMultiMatchQuery [FieldName "title^3", FieldName "content"] (QueryString q)
    search = mkHighlightSearch (Just query) highlights
    highlights = Highlights Nothing [FieldHighlight (FieldName "content") Nothing]

makeResult :: Config -> Hit Document -> WikiSearchResult
makeResult Config{..} hit = WikiSearchResult title url hl (hitScore hit)
  where
    hl = T.unlines . map toMarkdown <$> (hitHighlight hit >>= M.lookup "content")
    docId = T.pack . show . hitDocId $ hit
    url = maybe cfgWikiURL (makeDocUrl cfgWikiURL) $ hitSource hit
    title = fromMaybe "No title" (docTitle <$> hitSource hit)

makeDocUrl :: URI -> Document -> URI
makeDocUrl baseUrl Document{..} = name `relativeTo` baseUrl
  where
    Just name = parseRelativeReference (dropExtension docFileName)

toMarkdown :: Text -> Text
toMarkdown = T.replace "<em>" "*" . T.replace "</em>" "*"
