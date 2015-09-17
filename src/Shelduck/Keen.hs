{-# LANGUAGE OverloadedStrings #-}

module Shelduck.Keen (
  sendToKeen
)  where

import           Control.Lens              hiding ((.=))
import           Control.Monad             (void)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import qualified Data.ByteString.Lazy      as L
import           Data.Text
import           Data.Traversable
import           Shelduck.Internal
import qualified Network.Wreq              as W
import           System.Environment

opts = W.defaults & W.header "Accept" .~ ["application/json"]
                  & W.header "Content-Type" .~ ["application/json"]

sendToKeen :: Text -> Bool -> MaybeT IO ()
sendToKeen t b = do
  k <- lift keenEndpoint
  case k of
    Just k' -> lift $ void $ sendToKeen' t b k'
    _ -> return ()
  return ()

sendToKeen' :: Text -> Bool -> String -> IO (W.Response L.ByteString)
sendToKeen' t b k = W.postWith opts k (toJSON $ object ["type" .= t, "pass" .= b])
