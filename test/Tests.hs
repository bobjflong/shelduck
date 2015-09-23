{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.Maybe
import           Data.Text
import qualified Network.Wreq               as W
import           Shelduck
import           Shelduck.Internal
import           Shelduck.Templating
import           System.Environment
import           Test.Hspec
import           Text.Regex

main :: IO ()
main = hspec $ do
  describe "templating" $
    it "substitutes random numbers at runtime" $ do
      let regex = mkRegex "{\"foo\":\"bar[0-9a-zA-Z\\-]+\"}"
      t <- template "{\"foo\":\"bar{{random}}\"}"
      matchRegex regex (unpack t) `shouldBe` Just []
  describe "reading results" $ do
    it "detects success" $ do
      r <- newTVarIO (Just "foo") :: IO (TVar TopicResult)
      let req = blank & requestEndpoint .~ "x"
                      & requestOpts .~ W.defaults
                      & requestParameters .~ object []
                      & requestTopic .~ "foo"
          handler = doHandle req
      result <- runReaderT handler r
      result `shouldBe` True
    it "detects failures" $ do
      r <- newTVarIO (Just "bar") :: IO (TVar TopicResult)
      let req = blank & requestEndpoint .~ "x"
                      & requestOpts .~ W.defaults
                      & requestParameters .~ object []
                      & requestTopic .~ "foo"
          handler = doHandle req
      result <- runReaderT handler r
      result `shouldBe` False
  describe "writing results" $
    it "works" $ do
      r <- newTVarIO Nothing :: IO (TVar TopicResult)
      record (Just "foo") r
      result <- readTVarIO r
      result `shouldBe` Just "foo"
  describe "polling for TVar results" $ do
    it "works" $ do
      r <- newTVarIO Nothing :: IO (TVar TopicResult)
      let c = const $ return True
      let f = return "Hello world" :: IO String
      (_, t) <- pollingIO 1 r c f
      t `shouldBe` "Hello world"
    it "gives up" $ do
      r <- newTVarIO Nothing :: IO (TVar TopicResult)
      let c = const $ return False
      let f = return "Hello world" :: IO String
      (i, t) <- pollingIO 10 r c f
      t `shouldBe` "Hello world"
      i `shouldBe` 0
  describe "Keen" $
    it "should construct Keen urls" $ do
      setEnv "KEEN_PROJECT_ID" "foo"
      setEnv "KEEN_API_KEY" "bar"
      k <- keenEndpoint
      fromJust k `shouldBe` "https://api.keen.io/3.0/projects/foo/events/shelduck?api_key=bar"
  describe "Slack" $
    it "should encode Slack payloads" $ do
      let report = SlackTestReport "foo" True
      encode report `shouldBe` "{\"text\":\"Topic: foo, pass: True\"}"
