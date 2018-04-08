{-# LANGUAGE NoOverloadedStrings, RecordWildCards, LambdaCase #-}

import Development.Shake
import Development.Shake.FilePath
import qualified Data.Text as T
import Data.Text (Text)
import Data.Aeson
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Data.List.Split (splitOn)
import Data.Time.LocalTime (ZonedTime(..))
import qualified Data.ByteString.Char8 as S8

import Config
import Search

main :: IO ()
main = do
  config@Config{..} <- getConfigFromEnv
  let search = makeDocSearch config

  shakeArgs shakeOptions{shakeFiles=cfgBuildDir} $ do
    sources <- liftIO $ findSourceDocs cfgDocsDir

    let docHtmls = [cfgBuildDir </> md -<.> "html" | md <- sources]
        indexedHtmls = [cfgBuildDir </> md -<.> "indexed" | md <- sources]
        sourceFor :: FilePath -> FilePath
        sourceFor = getSourceFor cfgBuildDir cfgDocsDir sources

    want $ docHtmls ++ indexedHtmls

    cfgBuildDir ++ "//*.html" %> \out -> do
      let md = sourceFor out
      need [md]
      pandoc "html" ["--toc", "-s"] md out

    cfgBuildDir ++ "//*.txt" %> \out -> do
      let md = sourceFor out
      need [md]
      pandoc "plain" [] md out

    let metaTemplate = cfgBuildDir </> "meta.template"
    metaTemplate %> \out -> writeFile' out "$meta-json$"

    cfgBuildDir ++ "//*.json" %> \out -> do
      let md = sourceFor out
      need [md, metaTemplate]
      metas <- gitMetas cfgDocsDir md
      pandoc "markdown" ([ "-s", "--template", metaTemplate] ++ metas) md out

    cfgBuildDir ++ "//*.indexed" %> \out -> do
      let md = sourceFor out
          html = out -<.> "html"
          txt = out -<.> "txt"
          meta = out -<.> "json"
      need [cfgBuildDir </> "elastic-index"]
      doc <- loadDocument config md txt html meta
      putNormal $ "Indexing " ++ (T.unpack (docId doc)) ++ " " ++ takeFileName md
      void . liftIO $ indexSearchDoc search doc
      writeFile' out (T.unpack (docId doc))

    cfgBuildDir </> "elastic-index" %> \out -> do
      putNormal "Setting up elasticsearch index"
      liftIO $ createDocIndex search
      writeFile' out (show (docSearchIdx search))

    let
      cleanIndex :: Action ()
      cleanIndex = do
        putNormal "Deleting elasticsearch index"
        liftIO $ deleteDocIndex search
        removeFilesAfter cfgBuildDir ["*.indexed"]

    phony "clean" $ do
      putNormal $ "Cleaning files in " ++ cfgBuildDir
      removeFilesAfter cfgBuildDir ["//*"]
      cleanIndex

    phony "clean-index" cleanIndex


loadDocument  :: Config -> FilePath -> FilePath -> FilePath -> FilePath -> Action Document
loadDocument Config{..} md txt html meta = do
  need [txt, html, meta]
  eitherDecodeStrict <$> liftIO (S8.readFile meta) >>= \case
    Right DocMetadata{..} -> makeDocument cfgDocsDir md
                             <$> readFile' txt
                             <*> readFile' html
                             <*> pure mdUpdated
                             <*> pure mdAuthorName
    Left e -> error e

pandoc :: String -> [String] -> FilePath -> FilePath -> Action ()
pandoc to args f out = command_ [] "pandoc" $ ["-f", from, "-t", to] ++ args ++ ["-o", out, f]
  where from = case takeExtension f of
                 "md" -> "gfm"
                 "org" -> "org"
                 "rst" -> "rst"
                 _ -> "markdown"

findSourceDocs :: FilePath -> IO [FilePath]
findSourceDocs dir = filter notHiddenFile <$> getDirectoryFilesIO dir ["//*." ++ f | f <- formats]
  where formats = ["md", "org", "rst", "txt"]

notHiddenFile :: FilePath -> Bool
notHiddenFile = not . any isHidden . splitPath
  where isHidden = (\p -> p == "." || p == "_") . take 1

getSourceFor :: FilePath -> FilePath -> [FilePath] -> FilePath -> FilePath
getSourceFor buildDir dir sources out = dir </> (lookup' . baseFile $ out)
  where
    baseFile = makeRelative buildDir . dropExtension
    sources' = [(dropExtension f, f) | f <- sources]
    lookup' f = fromMaybe (f <.> "md") $ lookup f sources'

gitMetas :: FilePath -> FilePath -> Action [String]
gitMetas dir md = toMetas <$> command [Cwd dir] "git" log
  where
    log = ["log", "-1", "--format=%aI\t%an\t%ae", makeRelative dir md]
    toMetas (Stdout info) = concat $ case splitOn "\t" info of
      [updated, name, email] -> [meta "updated" updated, meta "author_name" name, meta "author_email" email]
      _ -> []
    meta k v = ["-M", k ++ "=" ++ v]

data DocMetadata = DocMetadata
  { mdUpdated :: ZonedTime
  , mdAuthorName :: Text
  , mdAuthorEmail :: Text
  } deriving (Show)

instance FromJSON DocMetadata where
  parseJSON = withObject "DocMetadata" $ \o ->
    DocMetadata <$> o .: (T.pack "updated")
    <*> o .: (T.pack "author_name")
    <*> o .: (T.pack "author_email")

{-
instance FromJSON ZonedTime where
  parseJSON = withString "ISO8601" $ \s -> case parseTimeM True defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S%z")) s of
    Just t -> pure t
    Nothing -> error "datetime parsing failed"
-}
