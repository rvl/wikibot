{-# LANGUAGE RecordWildCards #-}

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Database.V5.Bloodhound
import Network.HTTP.Client
import GHC.Generics
import qualified Data.Text as T
import Data.Text (Text)
import Data.Aeson
import Control.Monad (void, when)

import Config
import Search

main :: IO ()
main = do
  Right config@Config{..} <- getConfig "config.yml"
  let search = makeDocSearch config

  shakeArgs shakeOptions{shakeFiles=cfgBuildDir} $ do
    markdowns <- fmap (filter notHiddenFile) . liftIO $ getDirectoryFilesIO cfgDocsDir ["//*.md"]

    let docHtmls = [cfgBuildDir </> md -<.> "html" | md <- markdowns]
        indexedHtmls = [cfgBuildDir </> md -<.> "indexed" | md <- markdowns]
        sourceFor :: FilePath -> FilePath
        sourceFor out = cfgDocsDir </> (dropDirectory1 $ out -<.> "md")

    want $ docHtmls ++ indexedHtmls

    cfgBuildDir ++ "//*.html" %> \out -> do
      let md = sourceFor out
      need [md]
      cmd_ "pandoc -f markdown -t html -s -o" [out, md]

    cfgBuildDir ++ "//*.indexed" %> \out -> do
      let md = sourceFor out
          html = out -<.> "html"
      need [html, md, "_build/elastic-index"]
      doc <- loadDocument config md html
      putNormal $ "Indexing " ++ (T.unpack (docId doc))
      resp <- liftIO $ indexSearchDoc search doc
      writeFile' out (show resp)

    cfgBuildDir </> "elastic-index" %> \out -> do
      putNormal "Setting up elasticsearch index"
      resp <- liftIO $ createDocIndex search
      writeFile' out (show resp)

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


loadDocument  :: Config -> FilePath -> FilePath -> Action Document
loadDocument Config{..} md html = makeDocument cfgDocsDir md <$> readFile' html

notHiddenFile :: FilePath -> Bool
notHiddenFile = not . any isHidden . splitPath
  where isHidden = (== ".") . take 1
