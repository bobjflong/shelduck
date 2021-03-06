{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

module Shelduck.Internal where

import           Control.Concurrent
import           Control.Concurrent.STM.TVar
import           Control.Conditional
import           Control.Lens                hiding ((.=))
import           Control.Monad
import           Control.Monad.State.Strict
import           Control.Monad.STM
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8  as BL
import           Data.HashMap.Strict
import           Data.Text
import           Data.Time.Clock.POSIX
import qualified Network.Wreq                as W
import           Shelduck.Configuration
import           System.Directory
import           System.Environment
import qualified System.IO                   as IO

data WebhookRequest = WebhookRequest {
  _requestEndpoint   :: Text,
  _requestOpts       :: W.Options,
  _requestParameters :: Value,
  _requestTopic      :: Text
}

$(makeLenses ''WebhookRequest)

blank :: WebhookRequest
blank = WebhookRequest mempty W.defaults (object []) mempty

logFile :: IO FilePath
logFile = do
  home <- getHomeDirectory
  return $ mconcat [home, "/shelduck.log"]

type TopicResult = Maybe Text
type RequestData = (WebhookRequest, Text, Text)

data TestRunData = TestRunData {
  _logs        :: [Value],
  _topicResult :: TVar TopicResult
}
$(makeLenses ''TestRunData)

type TestRun a = StateT TestRunData IO a

record :: Maybe Text -> TVar (Maybe Text) -> IO ()
record t r = atomically $ writeTVar r t

doRetry :: RequestData -> (RequestData -> TestRun b) -> TestRun ()
doRetry r c = get >>=
  \t -> do
    currentResult <- lift $ atomically (readTVar $ t ^. topicResult)
    case currentResult of
      Nothing -> logs <>= [jsonRetry] >> c r >> void (lift $ threadDelay retryWait)
      _ -> return ()
    where jsonRetry = object ["retry" .= True]

pollingIO :: Int -> TVar a -> (TVar a -> IO Bool) -> IO b -> IO (Int, b)
pollingIO c t x i = temporaryFailure >>= continue
  where continue f = if f then tryAgain else finish
        tryAgain = threadDelay pollTime >> pollingIO (c - 1) t x i
        finish = (c,) <$> i
        temporaryFailure = ((&& c > 0) . not) <$> x t

keenEndpoint :: IO (Maybe String)
keenEndpoint = do
  p <- lookupEnv "KEEN_PROJECT_ID"
  a <- lookupEnv "KEEN_API_KEY"
  return $ do
    base <- pure "https://api.keen.io/3.0/projects/"
    p' <- p
    middle <- pure "/events/shelduck?api_key="
    a' <- a
    return . mconcat $ [base, p', middle, a']

data SlackTestReport = SlackTestReport {
  topic :: Text,
  pass  :: Bool
}

instance ToJSON SlackTestReport where
  toJSON SlackTestReport{..} = object ["text" .= mconcat ["Topic: ", topic, ", pass: ", (pack . show) pass]]

info :: Value -> IO ()
info v = do
  file <- logFile
  ifM (doesFileExist file) (appendToLog file (encode v)) logWarn

appendToLog :: FilePath -> BL.ByteString -> IO ()
appendToLog file value = BL.appendFile file (mconcat [value, "\n"])

logWarn :: IO ()
logWarn = IO.hPutStr IO.stderr "No log file found"
