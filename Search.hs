{-# LANGUAGE DeriveGeneric, PackageImports, RecordWildCards, OverloadedStrings, Rank2Types, DeriveDataTypeable, ScopedTypeVariables #-}

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
  , Paging(..)
  , IndexingError(..)
  , SearchError(..)
  ) where

import Database.V5.Bloodhound
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusIsSuccessful)
import GHC.Generics hiding (from)
import Data.Text (Text)
import qualified Data.Text as T
import Crypto.Hash
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as LS8
import Data.Aeson
import Development.Shake.FilePath
import Control.Monad (void, when)
import System.FilePath.Posix (dropExtension, takeBaseName)
import qualified Data.Map.Lazy as M
import Network.URI (URI, relativeTo, parseRelativeReference)
import Control.Monad.Catch
import Data.Typeable
import Data.Time.LocalTime (ZonedTime(..))
import Data.Maybe (catMaybes)

import Config

data Document = Document { docId        :: Text
                         , docContent   :: Text
                         , docHTML      :: Text
                         , docFileName  :: FilePath
                         , docTitle     :: Text
                         , docUpdatedBy :: Text
                         , docUpdatedOn :: ZonedTime
                         } deriving (Generic, Show)

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
               , "updated_on" .= object [ "type" .= ("date" :: Text)
                                        , "format" .= ("strict_date_optional_time||epoch_millis" :: Text ) ]
               ]
      ]

makeDocument :: FilePath -> FilePath -> String -> String -> ZonedTime -> Text -> Document
makeDocument base doc txt html dt author = Document
  { docId = T.pack $ makeDocumentId name
  , docContent = T.pack txt
  , docHTML = T.pack html
  , docFileName = name
  , docTitle = titleFromFilename doc
  , docUpdatedBy = author
  , docUpdatedOn = dt
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

createDocIndex :: DocSearch -> IO LS8.ByteString
createDocIndex DocSearch{..} = docSearchRun createIndex >>= handleIndexResponse
  where
    createIndex = do
      exists <- indexExists docSearchIdx
      when exists . void $ deleteIndex docSearchIdx
      res <- createIndexWith settings numShards docSearchIdx
      void $ putMapping docSearchIdx docSearchMapping DocumentMapping
      pure res
    replicaCount = 0
    numShards = 1
    settings = [ NumberOfReplicas (ReplicaCount replicaCount)
               , AnalysisSetting wikiAnalysis ]

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

indexSearchDoc :: DocSearch -> Document -> IO LS8.ByteString
indexSearchDoc DocSearch{..} doc = docSearchRun index >>= handleIndexResponse
  where
    index = indexDocument docSearchIdx docSearchMapping defaultIndexDocumentSettings doc (DocId (docId doc))

handleIndexResponse :: Reply -> IO LS8.ByteString
handleIndexResponse r | statusIsSuccessful (responseStatus r) = pure body
                      | otherwise = throwM $ IndexingError body
  where body = responseBody r

data IndexingError = IndexingError LS8.ByteString
  deriving (Typeable)
instance Show IndexingError where
  show (IndexingError r) = "IndexingError: " ++ show r
instance Exception IndexingError

data SearchError = SearchErrorBadResponse String -- ^ Elasticsearch JSON response was the wrong type
                 | SearchErrorHttp HttpException -- ^ Error connecting to Elasticsearch by HTTP
                 | SearchErrorFailure HttpExceptionContent -- ^ Call to Elasticsearch failed
  deriving (Typeable)
instance Show SearchError where
  show (SearchErrorBadResponse r) = "Could not decode Elasticsearch response: " ++ show r
  show (SearchErrorHttp exc) = "Could not connect to Elasticsearch: " ++ show exc
  show (SearchErrorFailure exc) = "Call to Elasticsearch failed: " ++ show exc
instance Exception SearchError

data WikiSearchResult = WikiSearchResult
  { wikiResDoc :: Document
  , wikiResURL :: URI
  , wikiResHighlight :: Maybe Text
  , wikiResScore :: Maybe Double
  } deriving (Generic, Show)

doSearch :: DocSearch -> Paging -> Text -> IO (Int, [WikiSearchResult])
doSearch ds@DocSearch{..} p q = do
  res <- handleSearch ds $ searchByIndex docSearchIdx (withPaging p search)
  pure $ makeResults $ searchHits res
  where
    makeResults :: SearchHits Document -> (Int, [WikiSearchResult])
    makeResults SearchHits{..} = (hitsTotal, catMaybes $ map (makeResult docSearchConfig) hits)
    query = QueryMultiMatchQuery $ mkMultiMatchQuery [FieldName "title^3", FieldName "content"] (QueryString q)
    search = mkHighlightSearch (Just query) highlights
    highlights = Highlights Nothing [FieldHighlight (FieldName "content") Nothing]

handleSearch :: forall a. FromJSON a => DocSearch -> BH IO Reply -> IO (SearchResult a)
handleSearch DocSearch{..} search = catch (docSearchRun search) handleExc >>= handleReply
  where
    handleReply :: Reply -> IO (SearchResult a)
    handleReply r | statusIsSuccessful (responseStatus r) = case eitherDecode (responseBody r) of
                           Right res -> pure res
                           Left err -> throwM $ SearchErrorBadResponse err
                  | otherwise = do
                      let chunk = LS8.take 1024 $ responseBody r
                      let res' = fmap (const ()) r
                      throwM $ SearchErrorFailure $ StatusCodeException res' (LS8.toStrict chunk)
    handleExc :: HttpException -> IO Reply
    handleExc = throwM . SearchErrorHttp

makeResult :: Config -> Hit Document -> Maybe WikiSearchResult
makeResult Config{..} hit = make <$> hitSource hit
  where
    make doc = WikiSearchResult doc url hl (hitScore hit)
    hl = T.unlines . map toMarkdown <$> (hitHighlight hit >>= M.lookup "content")
    docId = T.pack . show . hitDocId $ hit
    url = maybe cfgWikiURL (makeDocUrl cfgWikiURL) $ hitSource hit

makeDocUrl :: URI -> Document -> URI
makeDocUrl baseUrl Document{..} =
  case parseRelativeReference (dropExtension docFileName) of
    Just name -> name `relativeTo` baseUrl
    Nothing -> baseUrl

toMarkdown :: Text -> Text
toMarkdown = T.replace "<em>" "*" . T.replace "</em>" "*"

data Paging = Paging { pagingFrom :: Int, pagingSize :: Int }

withPaging :: Paging -> Search -> Search
withPaging (Paging f s) search = search { from = From f, size = Size s }
