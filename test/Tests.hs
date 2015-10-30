{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.Maybe
import qualified Network.Wreq               as W
import           Shelduck
import           Shelduck.Alarming
import           Shelduck.Internal
import           Shelduck.LogParser
import           System.Environment
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "reading results" $ do
    it "detects success" $ do
      r <- newTVarIO (Just "foo") :: IO (TVar TopicResult)
      let req = blank & requestEndpoint .~ "x"
                      & requestOpts .~ W.defaults
                      & requestParameters .~ object []
                      & requestTopic .~ "foo"
          handler = checkAssertion req
      result <- runReaderT handler r
      result `shouldBe` True
    it "detects failures" $ do
      r <- newTVarIO (Just "bar") :: IO (TVar TopicResult)
      let req = blank & requestEndpoint .~ "x"
                      & requestOpts .~ W.defaults
                      & requestParameters .~ object []
                      & requestTopic .~ "foo"
          handler = checkAssertion req
      result <- runReaderT handler r
      result `shouldBe` False
  describe "retries" $ do
    it "works" $ do
      r <- newTVarIO Nothing :: IO (TVar TopicResult)
      let requestData = (blank, mempty, mempty)
          continue :: RequestData -> ReaderT (TVar TopicResult) IO ()
          continue _ = void $ lift (atomically $ writeTVar r (Just "done"))
      runReaderT (doRetry requestData continue) r
      result <- readTVarIO r
      result `shouldBe` Just "done"
    it "does not retry unnecessarily" $ do
      r <- newTVarIO (Just "untouched") :: IO (TVar TopicResult)
      let requestData = (blank, mempty, mempty)
          continue :: RequestData -> ReaderT (TVar TopicResult) IO ()
          continue _ = void $ lift (atomically $ writeTVar r (Just "done"))
      runReaderT (doRetry requestData continue) r
      result <- readTVarIO r
      result `shouldBe` Just "untouched"
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
  describe "Log parsing" $
    it "should calculate verbs" $ do
      let logLine = toLogLine "{\"params\":\"foo\"}"
      (show . verb) logLine `shouldBe` "post request made"
      let logLine = toLogLine "{\"retry\":true}"
      (show . verb) logLine `shouldBe` "retry performed"
      let logLine = toLogLine "{\"bad\": "
      (show . verb) logLine `shouldBe` "unknown action: \"{\\\"bad\\\": \""
      let logLine = toLogLine "{\"foo\":\"bar\"}"
      (show . verb) logLine `shouldBe` "log line contained no action"
  describe "Alarming" $
    it "should alarm when the threshold is reached" $ do
      let badRun = defaultDefinitionListRun & assertionFailedCount .~ 5
                                            & assertionCount .~ 10
      shouldAlarm badRun `shouldBe` True
      let goodRun = defaultDefinitionListRun & assertionFailedCount .~ 4
                                             & assertionCount .~ 10
      shouldAlarm goodRun `shouldBe` False
