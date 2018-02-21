{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase #-}
module SearchSpec where

import           Test.Hspec
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteArray.Encoding as B (Base (..), convertToBase)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Monoid
import Control.Error
import Control.Monad (join)
import Data.Aeson

import Search
import Config

testConfig :: Config
testConfig = undefined

setupIndex :: IO DocSearch
setupIndex = do
  let ds = makeDocSearch testConfig
  deleteDocIndex ds
  return ds

teardownIndex :: DocSearch -> IO ()
teardownIndex ds = deleteDocIndex ds

spec :: Spec
spec = do
  beforeAll setupIndex $ afterAll teardownIndex $
    describe "Index document" $ do
      it "indexes something properly" $ do
        pure ()
      it "evaluates and creates build" $ do
        pure ()
