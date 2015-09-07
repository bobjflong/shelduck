{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text
import           Templating
import           Test.Hspec
import           Text.Regex
import Scalpel
import Data.Aeson
import           Control.Concurrent.STM
import           Control.Monad.Trans.Reader

main :: IO ()
main = hspec $ do
  describe "templating" $
    it "substitutes random numbers at runtime" $ do
      let regex = mkRegex "{\"foo\":\"bar[0-9.e\\-]+\"}"
      t <- template "{\"foo\":\"bar{{random}}\"}"
      matchRegex regex (unpack t) `shouldBe` Just []
  describe "reading results" $ do
    it "detects success" $ do
      r <- newTVarIO (Just "foo") :: IO (TVar TopicResult)
      let req = WebhookRequest "x" (object []) "foo"
          handler = doHandle req
      result <- runReaderT handler r
      result `shouldBe` True
    it "detects failures" $ do
      r <- newTVarIO (Just "bar") :: IO (TVar TopicResult)
      let req = WebhookRequest "x" (object []) "foo"
          handler = doHandle req
      result <- runReaderT handler r
      result `shouldBe` False
  describe "writing results" $
    it "works" $ do
      r <- newTVarIO Nothing :: IO (TVar TopicResult)
      record (Just "foo") r
      result <- readTVarIO r
      result `shouldBe` Just "foo"
