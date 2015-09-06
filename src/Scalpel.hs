{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Scalpel where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM.TVar
import           Control.Lens                hiding ((.=))
import           Control.Lens.TH
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Monoid
import           Data.Text
import qualified Network.Wreq                as W
import           Rainbow
import           Web.Spock.Safe

endpoint :: String
endpoint = "http://requestb.in/zsczv6zs"

exampleRequest = WebhookRequest endpoint (object ["foo" .= ("bar" :: Text)]) "baz"

opts c = W.defaults & W.header "Accept" .~ ["application/json"]
                    & W.header "Content-Type" .~ ["application/json"]

type TopicResult = Maybe Text

data WebhookRequest = WebhookRequest {
  _requestEndpoint   :: String,
  _requestParameters :: Value,
  _requestTopic      :: Text
}

$(makeLenses ''WebhookRequest)

performRequest :: WebhookRequest -> IO ()
performRequest w = post >>= log >> wait
  where post = W.post (w ^. requestEndpoint) (w ^. requestParameters)
        log r = do
          let response = (pack . show) $ r ^. W.responseStatus
          putChunkLn $ chunk response & fore green
        wait = void $ threadDelay 5000000

info :: Text -> IO ()
info x = putChunkLn $ chunk x & fore blue

start :: IO ()
start = do
  info "Creating TVar"
  result <- newTVarIO Nothing :: IO (TVar TopicResult)
  async $ server result
  getLine
  return ()

server :: TVar TopicResult -> IO ()
server t = do
  info "Starting server to listen for webhooks"
  runSpock 8080 $ spockT id $
                  post root $ do
                       responseBody <- body
                       let topic  = responseBody ^? key "topic" & (pack . show)
                       text topic
