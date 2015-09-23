{-# LANGUAGE OverloadedStrings #-}

module Shelduck.Slack (
  SlackTestReport(..),
  sendToSlack
)  where

import           Control.Lens       hiding ((.=))
import           Control.Monad
import           Data.Aeson
import           Data.Text
import qualified Network.Wreq       as W
import           Shelduck.Internal
import           System.Environment

sendToSlack :: Text -> Bool -> IO ()
sendToSlack t b = do
  e <- lookupEnv "SLACK_WEBHOOK_URL"
  case e of
    Nothing -> return ()
    (Just endpoint) -> W.post endpoint (encode $ SlackTestReport t b) & void
