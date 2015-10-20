{-# LANGUAGE OverloadedStrings #-}

module Shelduck.Keen (
  sendToKeen
)  where

import           Control.Lens              hiding ((.=))
import           Control.Monad             (void)
import           Data.Aeson
import qualified Data.ByteString.Lazy      as L
import           Data.Text
import           Shelduck.Internal
import qualified Network.Wreq              as W

opts :: W.Options
opts = W.defaults & W.header "Accept" .~ ["application/json"]
                  & W.header "Content-Type" .~ ["application/json"]

sendToKeen :: Text -> Bool -> IO ()
sendToKeen t b = do
  k <- keenEndpoint
  case k of
    Just k' -> void $ sendToKeen' t b k'
    _ -> return ()

sendToKeen' :: Text -> Bool -> String -> IO (W.Response L.ByteString)
sendToKeen' t b k = W.postWith opts k (toJSON $ object ["type" .= t, "pass" .= b])
