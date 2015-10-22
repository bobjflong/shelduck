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

import           Control.Concurrent.Async
import           Control.Concurrent.STM.TVar
import           Control.Lens                hiding ((.=))
import           Control.Monad
import           Control.Monad.STM
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.ByteString.Lazy        (toStrict)
import qualified Data.ByteString.Lazy        as L
import           Data.Maybe
import           Data.Text
import           Data.Text.Encoding
import qualified Network.Wreq                as W
import           Shelduck.Configuration
import           Shelduck.Internal
import           Shelduck.Keen
import           Shelduck.Slack
import           Shelduck.Templating
import           Shelly
import           Web.Spock.Safe

data TimedResponse = TimedResponse {
  _response :: W.Response L.ByteString,
  _timing   :: Int
} deriving (Show)

$(makeLenses ''TimedResponse)

type Topic = Text

doPost :: RequestData -> ReaderT a IO (W.Response L.ByteString)
doPost (w, e, p) = lift $ do
  (info . json) e
  W.postWith (w ^. requestOpts) (unpack e) (encodeUtf8 p)
  where json x = object ["endpoint" .= x, "params" .= (w ^. requestOpts . W.params)]

doLog :: W.Response L.ByteString -> ReaderT a IO (W.Response L.ByteString)
doLog r = lift ((info . json . pack . show) (r ^. W.responseStatus)) >> return r
  where json s = object ["status" .= s]

doWait :: RequestData -> Int -> W.Response L.ByteString -> ReaderT (TVar TopicResult) IO TimedResponse
doWait d c x = do
  t <- ask
  (c, r) <- lift $ pollingIO c t predicate (return x)
  doRetry d doPost >> return (TimedResponse r c)
  where predicate t = fmap isJust (readTVarIO t)

doHandle :: WebhookRequest -> ReaderT (TVar TopicResult) IO Bool
doHandle w = ask >>=
  \t -> lift $ do
    b <- fromMaybe mempty <$> atomically (readAndWipe t)
    pass <- checkTopic b (w ^. requestTopic)
    sendToServices (w ^. requestTopic) pass
    return pass

sendToServices :: Topic -> Bool -> IO ()
sendToServices t b = do
  sendToKeen t b
  unless b $ sendToSlack t b

readAndWipe :: TVar TopicResult ->  STM TopicResult
readAndWipe t = do
  b' <- readTVar t
  writeTVar t Nothing
  return b'

doLogTimings :: Int -> TimedResponse -> IO ()
doLogTimings i t = void $ info $ object ["duration" .= time]
  where time = (i - (t ^. timing)) * pollTime

performRequest :: WebhookRequest -> ReaderT (TVar TopicResult) IO TimedResponse
performRequest w = doTemplating >>= \req ->
                   doPost req >>=
                   doLog >>=
                   doWait req polls >>=
                   (\x ->
                     lift (doLogTimings polls x) >>
                     doHandle w >> return x)
  where doTemplating = lift $ do
          e <- template (w ^. requestEndpoint)
          p <- template ((decodeUtf8 . toStrict . encode) $ w ^. requestParameters)
          return (w, e, p)

checkTopic :: Topic -> Topic -> IO Bool
checkTopic b t =
  if b == t
  then info (object ["good_topic" .= showResult b]) >> return True
  else info (object ["bad_topic" .= showResult b]) >> return False
  where showResult = pack . show

ngrok :: IO ()
ngrok = shelly $ verbosely $ run "ngrok" ["start", "shelduck"] >>= (liftIO . info . json)
  where json x = object ["ngrok_message" .= x]

server :: TVar TopicResult -> IO ()
server t = void $ concurrently ngrok $ do
  info "Starting server to listen for webhooks"
  runSpock 8080 $ spockT id $
                  post root $ do
                       responseBody <- body
                       let topic  = responseBody ^? key "topic"
                       lift $ case topic of
                         Just (String s) -> record (Just s) t
                         _ -> record Nothing t
                       r <- lift $ readTVarIO t
                       text $ r & (pack . show)
