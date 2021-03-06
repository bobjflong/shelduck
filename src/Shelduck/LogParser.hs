{-# LANGUAGE OverloadedStrings #-}

module Shelduck.LogParser where

import           Control.Lens
import           Data.Aeson
import           Data.ByteString.Lazy
import           Data.HashMap.Strict
import           Data.Maybe
import qualified Data.Text            as T
import           Data.Text.Encoding
import           Prelude              hiding (lookup)

data LogLine = Data (HashMap T.Text Value) | UnParseable T.Text

data LogLineAction = PostRequestMade
                     | PostResponseReceived
                     | RoundTrip
                     | CorrectTopicReceived
                     | IncorrectTopicReceived
                     | UnknownAction T.Text
                     | Retry
                     | NoAction
                     | HttpExceptionDuringTest

instance Show LogLineAction where
  show PostRequestMade = "post request made"
  show PostResponseReceived = "post response received"
  show RoundTrip = "finished waiting for webhook"
  show CorrectTopicReceived = "correct topic received"
  show IncorrectTopicReceived = "incorrect topic received"
  show Retry = "retry performed"
  show (UnknownAction b) = mconcat ["unknown action: ", show b]
  show NoAction = "log line contained no action"
  show HttpExceptionDuringTest = "http exception detected"

verb :: LogLine -> LogLineAction
verb (UnParseable b) = UnknownAction b
verb (Data l) = resolve logDictionary
 where logDictionary = [ lookup "params" l *> pure PostRequestMade
                       , lookup "status" l *> pure PostResponseReceived
                       , lookup "duration" l *> pure RoundTrip
                       , lookup "good_topic" l *> pure CorrectTopicReceived
                       , lookup "bad_topic" l *> pure IncorrectTopicReceived
                       , lookup "retry" l *> pure Retry
                       , lookup "http_exception" l *> pure HttpExceptionDuringTest
                       ]

resolve :: [Maybe LogLineAction] -> LogLineAction
resolve x = catMaybes x ^? ix 0 & fromMaybe NoAction

toLogLine :: T.Text -> LogLine
toLogLine x = case decode ((fromStrict . encodeUtf8) x) of
                Nothing -> UnParseable x
                (Just p) -> Data p
