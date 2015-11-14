{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Shelduck (
    requestTopic,
    requestEndpoint,
    requestOpts,
    requestParameters,
    Shelduck.response,
    timing,
    testResult,
    info,
    TopicResult,
    server,
    WebhookRequest,
    performRequest,
    blank,
    checkAssertion,
    handleHTTPError,
    DefinitionListRun,
    assertionCount,
    assertionFailedCount,
    definitionListLogs,
    flush,
    defaultDefinitionListRun,
    TimedResponse,
    AssertionResult(..),
    runAssertion,
    TestRunData(..)
  ) where

import           Control.Concurrent.Async
import           Control.Concurrent.STM.TVar
import           Control.Lens                hiding ((.=))
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.State.Strict
import           Control.Monad.STM
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.ByteString.Lazy        (toStrict)
import qualified Data.ByteString.Lazy        as L
import           Data.Maybe
import           Data.Text
import qualified Data.Text                   as T
import           Data.Text.Encoding
import qualified Data.Text.IO                as TIO
import qualified Data.Text.Lazy              as TL
import qualified Data.Text.Lazy.Encoding     as LTE
import           Network.HTTP.Client         (HttpException)
import qualified Network.Wreq                as W
import           Shelduck.Configuration
import           Shelduck.Internal
import           Shelduck.Keen
import           Shelduck.Slack
import           Shelduck.Templating
import           Shelly                      hiding (get)
import           System.Environment
import           Web.Spock.Safe              hiding (get)

data AssertionResult = Pending | Passed | Failed deriving (Show, Eq)

fromBool :: Bool -> AssertionResult
fromBool True = Passed
fromBool False = Failed

data TimedResponse = TimedResponse {
  _response   :: Maybe (W.Response L.ByteString),
  _timing     :: Int,
  _testResult :: AssertionResult
} deriving (Show)

$(makeLenses ''TimedResponse)

type Topic = Text

doPost :: RequestData -> TestRun (W.Response L.ByteString)
doPost (w, e, p) = logs <>= [json e] >> lift req
  where json x = object ["endpoint" .= x, "params" .= p, "headers" .= (w ^. requestOpts . W.headers & show)]
        req = W.postWith (w ^. requestOpts) (unpack e) (encodeUtf8 p)

doLog :: W.Response L.ByteString -> TestRun (W.Response L.ByteString)
doLog r = do
  logs <>= [json . pack . show $ r ^. W.responseStatus . W.statusCode]
  return r
  where json s = object ["status" .= s]

doWait :: RequestData -> Int -> W.Response L.ByteString -> TestRun TimedResponse
doWait d c x = do
  t <- get
  (c, r) <- lift $ pollingIO c (t ^. topicResult) predicate (return x)
  doRetry d doPost
  return (TimedResponse (pure r) c Pending)
  where predicate t = fmap isJust (readTVarIO t)

checkAssertion :: WebhookRequest -> TestRun Bool
checkAssertion w = get >>=
  \t -> do
    b <- lift $ fromMaybe mempty <$> atomically (readAndWipe $ t ^. topicResult)
    checkTopic b (w ^. requestTopic)

sendToServices :: Topic -> Bool -> IO ()
sendToServices t b = do
  sendToKeen t b
  unless b $ sendToSlack t b

readAndWipe :: TVar TopicResult ->  STM TopicResult
readAndWipe t = do
  b' <- readTVar t
  writeTVar t Nothing
  return b'

doLogTimings :: Int -> TimedResponse -> TestRun ()
doLogTimings i t = do
  logs <>= [object ["duration" .= time]]
  return ()
  where time = (i - (t ^. timing)) * pollTime

performRequest :: WebhookRequest -> TestRun TimedResponse
performRequest w = doTemplating >>= \req ->
                   doPost req >>=
                   doLog >>=
                   (doWait req polls >=> handleTestCompletion w >=> doFlush)
  where doTemplating = lift $ (,,) w
          <$> template (w ^. requestEndpoint)
          <*> template ((decodeUtf8 . toStrict . encode) $ w ^. requestParameters)

handleTestCompletion :: WebhookRequest -> TimedResponse -> TestRun TimedResponse
handleTestCompletion w x = do
  r <- checkAssertion w
  doLogTimings polls x
  lift $ sendToServices (w ^. requestTopic) r
  return (testResult .~ fromBool r $ x)

flush :: [Value] -> IO ()
flush vs = do
  file <- logFile
  TIO.appendFile file $ T.intercalate "\n" (fmap _encode vs) `mappend` "\n"
  where _encode = TL.toStrict . LTE.decodeUtf8 . encode

doFlush :: a -> TestRun a
doFlush x = get >>= \s -> lift (flush (s ^. logs)) >> return x

checkTopic :: Topic -> Topic -> TestRun Bool
checkTopic b t =
  if b == t
  then do
    logs <>= [object ["good_topic" .= showResult b]]
    return True
  else do
    logs <>= [object ["bad_topic" .= showResult b]]
    return False
  where showResult = pack . show

data DefinitionListRun = DefinitionListRun {
  _assertionCount       :: Integer,
  _assertionFailedCount :: Integer,
  _definitionListLogs   :: [Value]
} deriving (Show)

defaultDefinitionListRun = DefinitionListRun 0 0 []

$(makeLenses ''DefinitionListRun)

runAssertion :: (MonadIO m, MonadCatch m) => TVar TopicResult -> WebhookRequest -> StateT DefinitionListRun m TimedResponse
runAssertion t x = do
  assertionResult <- catch (liftIO runRequest) handleHTTPError
  assertionCount += 1
  if assertionResult ^. testResult == Failed
    then assertionFailedCount += 1
    else assertionFailedCount += 0
  return assertionResult
  where runRequest = evalStateT (performRequest x) (TestRunData [] t)

handleHTTPError :: (MonadIO m, MonadCatch m) => HttpException -> StateT DefinitionListRun m TimedResponse
handleHTTPError e = do
  assertionCount += 1
  assertionFailedCount += 1
  definitionListLogs <>= [errorJSON]
  return (TimedResponse Nothing 0 Failed)
  where errorJSON = object ["http_exception" .= show e]

ngrok :: IO ()
ngrok = do
  enableNgrok <- (== pure "true") <$> lookupEnv "ENABLE_NGROK"
  if enableNgrok
    then shelly $ verbosely $ run "ngrok" ["start", "shelduck"] >>= (liftIO . info . json)
    else info "skipping ngrok"
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
