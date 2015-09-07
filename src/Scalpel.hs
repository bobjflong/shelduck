{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Scalpel where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM.TVar
import           Control.Lens                hiding ((.=))
import           Control.Lens.TH
import           Control.Monad
import           Control.Monad.STM
import           Control.Monad.Trans.Class
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
import           Shelly
import           Templating
import           Web.Spock.Safe

data WebhookRequest = WebhookRequest {
  _requestEndpoint   :: String,
  _requestOpts       :: W.Options,
  _requestParameters :: Value,
  _requestTopic      :: Text
}

blank :: WebhookRequest
blank = WebhookRequest mempty W.defaults (object []) mempty

$(makeLenses ''WebhookRequest)

type TopicResult = Maybe Text

doPost :: (WebhookRequest, Text, Text) -> ReaderT a IO (W.Response L.ByteString)
doPost (w, e, p) = lift $ W.postWith (w ^. requestOpts) (unpack e) (encodeUtf8 p)

doLog :: W.Response L.ByteString -> ReaderT a IO ()
doLog r = lift ((success . pack . show) (r ^. W.responseStatus)) & void

doWait :: ReaderT a IO ()
doWait = lift (threadDelay fiveSecs) & void
  where fiveSecs = 5000000

doHandle :: WebhookRequest -> ReaderT (TVar TopicResult) IO Bool
doHandle w = ask >>=
  \t -> lift $ do
    b <- readTVarIO t
    handleSuccess (fromMaybe mempty b) (w ^. requestTopic)

performRequest :: WebhookRequest -> ReaderT (TVar TopicResult) IO Bool
performRequest w = doTemplating >>= doPost >>= doLog >> doWait >> doHandle w
  where doTemplating = lift $ do
          e <- template (pack $ w ^. requestEndpoint)
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
info x = putChunkLn $ chunk x & fore blue

success :: Text -> IO ()
success x = putChunkLn $ chunk x & fore green

ngrok :: IO ()
ngrok = shelly $ verbosely $ run "ngrok" ["start", "scalpel"] >>= (liftIO . info)

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

record :: Maybe Text -> TVar TopicResult -> IO ()
record t r = atomically $ writeTVar r t
