{-# LANGUAGE OverloadedStrings #-}

module SlackEvents
  ( dropBasePath
  ) where

import qualified Data.Text as T
import Data.List (isPrefixOf)
import Network.Wai (Middleware, Request(pathInfo))
import Network.URI

dropBasePath :: URI -> Middleware
dropBasePath base app req = app req'
  where
    req' | base' `isPrefixOf` parts = req { pathInfo = drop (length base') parts }
         | otherwise                = req
    parts = pathInfo req
    base' = filter (not . T.null) $ T.splitOn "/" (T.pack $ uriPath base)
