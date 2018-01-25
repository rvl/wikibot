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
  ds <- makeDocSearch testConfig
  deleteDocIndex ds
  return ds

teardownIndex :: DocSearch -> IO ()
teardownIndex ds = deleteDocIndex ds

spec :: Spec
spec = do
  beforeAll setupIndex $ afterAll teardownIndex $
    describe "Index document" $ do
      it "indexes something properly" $ \TestEnv{..} -> do
        res <- fmap join . runExceptT . withExceptT show $ runApp testEnv (checkJobset "hercules" "master")
        res `shouldSatisfy` isLeft
        let Left msg = res
        msg `shouldContain` "not exist"

      it "evaluates and creates build" $ \TestEnv{..} -> do
        res <- runExceptT . runApp testEnv $ do
          setUpJobset
          checkJobset "hercules" "master"
          -- check for existence of build
        res `shouldSatisfy` isRight

-- withTestApp :: ActionWith (App a -> IO (Either String a)) -> ActionWith TestEnv
-- withTestApp action = action (\TestEnv{..} -> runExceptT . withExceptT show . runApp testEnv)

setUpJobset :: App ()
setUpJobset = withHerculesConnection $ \c -> do
  let repoId = 111319765
      rev = "081ae8c2568f0948eafab88a0eb670edcd830eed"
      sha256 = "1fd64ba72b037944d7e57d3e07e189bd8fb293302e176586092c74519e1d0d1b"
  addUpdateGitHubRepos c [GithubRepo repoId "hercules" "rvl/hercules" "master" "https://github.com/rvl/hercules.git" True Nothing]
  branchId <- updateJobsetBranch c repoId "master" rev specJSON
  -- fixme: addSource c branchId "/nix/store/yqlzi43dw7ppirynwcqf4a1ll9v0w4jp-w1711jp1df86yl97j2g70ww9b4wx3b3b-backend" sha256
  return ()

specJSON :: Value
specJSON = object [ ("jobset" :: Text) .= ("release.nix" :: Text) ]
