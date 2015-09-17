{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Shelduck (
    requestTopic,
    requestEndpoint,
    requestOpts,
    requestParameters,
    Shelduck.response,
    timing,
    info,
    TopicResult,
    server,
    WebhookRequest,
    performRequest,
    blank,
    doHandle
  ) where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM.TVar
import           Control.Lens                hiding ((.=))
import           Control.Lens.TH
import           Control.Monad
import           Control.Monad.STM
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.ByteString.Lazy        (toStrict)
import qualified Data.ByteString.Lazy        as L
import           Data.Maybe
import           Data.Monoid
import           Data.Text
import           Data.Text.Encoding
import qualified Network.Wreq                as W
import           Rainbow
import           Shelduck.Configuration
import           Shelduck.Internal
import           Shelduck.Keen
import           Shelduck.Templating
import           Shelly
import           Web.Spock.Safe

data WebhookRequest = WebhookRequest {
  _requestEndpoint   :: Text,
  _requestOpts       :: W.Options,
  _requestParameters :: Value,
  _requestTopic      :: Text
}

blank :: WebhookRequest
blank = WebhookRequest mempty W.defaults (object []) mempty

$(makeLenses ''WebhookRequest)

data TimedResponse = TimedResponse {
  _response :: W.Response L.ByteString,
  _timing   :: Int
} deriving (Show)

$(makeLenses ''TimedResponse)

type TopicResult = Maybe Text

doPost :: (WebhookRequest, Text, Text) -> ReaderT a IO (W.Response L.ByteString)
doPost (w, e, p) = lift $ W.postWith (w ^. requestOpts) (unpack e) (encodeUtf8 p)

doLog :: W.Response L.ByteString -> ReaderT a IO (W.Response L.ByteString)
doLog r = lift ((info . pack . show) (r ^. W.responseStatus)) >> return r

doWait :: Int -> W.Response L.ByteString -> ReaderT (TVar TopicResult) IO TimedResponse
doWait c x = ask >>=
  \t -> lift $ do
    (c, r) <- pollingIO c t predicate (return x)
    return $ TimedResponse r c
  where predicate t = fmap isJust (readTVarIO t)

doHandle :: WebhookRequest -> ReaderT (TVar TopicResult) IO Bool
doHandle w = ask >>=
  \t -> lift $ do
    b <- fmap (fromMaybe mempty) $ atomically $ do
      b' <- readTVar t
      writeTVar t Nothing
      return b'
    pass <- handleSuccess b (w ^. requestTopic)
    handleAnalytics (w ^. requestTopic) pass
    return pass

handleAnalytics :: Text -> Bool -> IO ()
handleAnalytics t b = runMaybeT (sendToKeen t b) & void

doLogTimings :: Int -> TimedResponse -> IO ()
doLogTimings i t = void $ info $ mconcat ["Took approx: ", (pack . show) time, " microseconds..."]
  where time = (i - (t ^. timing)) * pollTime

performRequest :: WebhookRequest -> ReaderT (TVar TopicResult) IO TimedResponse
performRequest w = doTemplating >>=
                   doPost >>=
                   doLog >>=
                   doWait polls >>=
                   (\x -> lift (doLogTimings polls x) >> doHandle w >> return x)
  where doTemplating = lift $ do
          e <- template (w ^. requestEndpoint)
          p <- template ((decodeUtf8 . toStrict . encode) $ w ^. requestParameters)
          return (w, e, p)

handleSuccess :: Text -> Text -> IO Bool
handleSuccess b t =
  if b == t
  then success (mconcat ["    Good topic: ", showResult b]) >> return True
  else failure (mconcat ["    Bad topic: ", showResult b]) >> return False
  where showResult = pack . show

failure :: Text -> IO ()
failure x = putChunkLn $ chunk x & fore red

info :: Text -> IO ()
info x = putChunkLn $ chunk x & fore cyan

success :: Text -> IO ()
success x = putChunkLn $ chunk x & fore green

ngrok :: IO ()
ngrok = shelly $ verbosely $ run "ngrok" ["start", "shelduck"] >>= (liftIO . info)

server :: TVar TopicResult -> IO ()
server t = void $ concurrently ngrok $ do
  info "Starting server to listen for webhooks"
  runSpock 8080 $ spockT id $
                  post root $ do
                       responseBody <- body
                       let topic  = responseBody ^? key "topic"
                       lift $ case topic of
                         Just (String s) -> record (Just s) t
                         Nothing -> record Nothing t
                       r <- lift $ readTVarIO t
                       text $ r & (pack . show)
