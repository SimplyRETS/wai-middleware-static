{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.StaticSpec (spec) where

import           Test.Hspec hiding (shouldBe, shouldReturn)
import           Test.Hspec.Expectations.Lifted
import           Test.Hspec.Wai
import           Test.Mockery.Directory

import           Web.Scotty (scottyApp)
import           Network.Wai.Test (SResponse(..))
import           Network.HTTP.Types.Status

import           Network.Wai.Middleware.Static

spec :: Spec
spec = around_ inTempDirectory $ do
  describe "static" $ with (static `fmap` scottyApp (return ())) $ do
    before_ (writeFile "foo.html" "some content") $ do
      context "on HEAD" $ do
        it "serves static file" $ do
          request "HEAD" "/foo.html" [] "" `shouldReturn` SResponse {
            simpleStatus = status200
          , simpleHeaders = [("Content-Type", "text/html")]
          , simpleBody = "some content"
          }

      context "on GET" $ do
        it "serves static file" $ do
          get "/foo.html" `shouldReturn` SResponse {
            simpleStatus = status200
          , simpleHeaders = [("Content-Type", "text/html")]
          , simpleBody = "some content"
          }

      context "otherwise" $ do
        it "serves upstream" $ do
          post "/foo.html" "" `shouldReturn` SResponse {
            simpleStatus = status404
          , simpleHeaders = [("Content-Type", "text/html")]
           , simpleBody = "<h1>404: File Not Found!</h1>"
           }

  describe "staticWithOptions" $ with cachedApp $ do
    before_ (writeFile "foo.html" "some content") $ do
      it "sends a quoted weak ETag and accepts it on revalidation" $ do
        let etag = "W/\"94e66df8cd09d410c62d9e0dc59d3a884e458e05\""
        response <- get "/foo.html"
        lookup "ETag" (simpleHeaders response) `shouldBe` Just etag
        request "GET" "/foo.html" [("If-None-Match", etag)] ""
          `shouldRespondWith` 304 {matchHeaders = ["ETag" <:> etag], matchBody = ""}
  where
    cachedApp = do
      cache <- initCaching PublicStaticCaching
      staticWithOptions defaultOptions {cacheContainer = cache}
        `fmap` scottyApp (return ())
