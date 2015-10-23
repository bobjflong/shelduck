{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Shelduck.Internal where

import           Control.Concurrent
import           Control.Concurrent.STM.TVar
import           Control.Lens                hiding ((.=))
import           Control.Monad
import           Control.Monad.STM
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8  as BL
import           Data.Text
import qualified Network.Wreq                as W
import           Shelduck.Configuration
import           System.Directory
import           System.Environment

data WebhookRequest = WebhookRequest {
  _requestEndpoint   :: Text,
  _requestOpts       :: W.Options,
  _requestParameters :: Value,
  _requestTopic      :: Text
}

blank :: WebhookRequest
blank = WebhookRequest mempty W.defaults (object []) mempty

logFile :: IO FilePath
logFile = do
  home <- getHomeDirectory
  return $ mconcat [home, "/shelduck.log"]

$(makeLenses ''WebhookRequest)

type TopicResult = Maybe Text
type RequestData = (WebhookRequest, Text, Text)

record :: Maybe Text -> TVar (Maybe Text) -> IO ()
record t r = atomically $ writeTVar r t

doRetry :: RequestData -> (RequestData -> ReaderT (TVar TopicResult) IO b) -> ReaderT (TVar TopicResult) IO ()
doRetry r c = ask >>=
  \t -> do
    currentResult <- lift $ atomically (readTVar t)
    case currentResult of
      Nothing -> lift (info "Retrying...") >> c r >> void (lift $ threadDelay retryWait)
      _ -> return ()

pollingIO :: Int -> TVar a -> (TVar a -> IO Bool) -> IO b -> IO (Int, b)
pollingIO c t x i = temporaryFailure >>= \f -> if f then tryAgain else finish
  where tryAgain = threadDelay pollTime >> pollingIO (c - 1) t x i
        finish = i >>= \result -> return (c, result)
        temporaryFailure = x t >>= \p -> return $ not p && c > 0

keenEndpoint :: IO (Maybe String)
keenEndpoint = do
  p <- lookupEnv "KEEN_PROJECT_ID"
  a <- lookupEnv "KEEN_API_KEY"
  return $ do
    base <- Just "https://api.keen.io/3.0/projects/"
    p' <- p
    middle <- Just "/events/shelduck?api_key="
    a' <- a
    return $ base ++ p' ++ middle ++ a'

data SlackTestReport = SlackTestReport {
  topic :: Text,
  pass  :: Bool
}

instance ToJSON SlackTestReport where
  toJSON SlackTestReport{..} = object ["text" .= mconcat ["Topic: ", topic, ", pass: ", (pack . show) pass]]

info :: Value -> IO ()
info v = do
  file <- logFile
  BL.appendFile file (mconcat [encode v, "\n"])
