{-# LANGUAGE OverloadedStrings #-}

module Shelduck.IntercomDefinitions where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM.TVar
import           Control.Lens                hiding ((.=))
import           Control.Monad
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Reader
import           Data.Aeson                  (object, (.=))
import           Data.Aeson.Lens             as AL
import           Data.ByteString.Char8       (pack)
import qualified Data.ByteString.Lazy        as L
import qualified Data.Text                   as T
import           Data.Time.Clock.POSIX
import           Data.UUID
import           Data.UUID.V4
import qualified Network.Wreq                as W
import           Shelduck                    hiding (opts)
import           Shelduck.Alarming
import           System.Environment

opts = do
  timestamp <- round <$> getPOSIXTime
  threadId <- toString <$> nextRandom
  let xThreadId = mconcat ["id=\"", threadId, "\", timestamp=\"", show timestamp, "\""]
  appId <- getEnv "INTERCOM_APP_ID"
  appApiKey <- getEnv "INTERCOM_APP_API_KEY"
  return $ W.defaults & W.header "Accept" .~ ["application/json"]
                      & W.header "Content-Type" .~ ["application/json"]
                      & W.header "X-TraceId" .~ [pack xThreadId]
                      & W.auth ?~ W.basicAuth (pack appId) (pack appApiKey)

--
-- Example parameters for API requests
--
userId :: T.Text
userId = "55b26822ce97179e52001334"

userIdForConversation :: T.Text
userIdForConversation = "55b251b52a281e8b530009a1"

userEmail :: T.Text
userEmail = "bob+testuser17@intercom.io"

userType :: T.Text
userType = "user"

adminType :: T.Text
adminType = "admin"

commentType :: T.Text
commentType = "comment"

noteType :: T.Text
noteType = "note"

assignmentType :: T.Text
assignmentType = "assignment"

adminId :: T.Text
adminId = "25610"

assigneeId :: T.Text
assigneeId = "25610"

contactId :: T.Text
contactId = "55d306eb886c98f8110028dd"

hi :: T.Text
hi = "Hi :)"

tagName :: T.Text
tagName = "foo"

eventName :: T.Text
eventName = "shelduck-test-event"

run :: TVar TopicResult -> StateT DefinitionListRun IO ()
run t = void $ do
  lift $ threadDelay 5000000

  options <- grabOptions
  eventTimestamp <- lift $ round <$> getPOSIXTime
  go $ blank & requestEndpoint .~ "https://api.intercom.io/events"
             & requestOpts .~ options
             & requestParameters .~ object ["email" .= userEmail, "event_name" .= eventName, "created_at" .= (eventTimestamp :: Integer)]
             & requestTopic .~ "event.created"

  options <- grabOptions
  contactResp <- go $ blank & requestEndpoint .~ "https://api.intercom.io/contacts"
                            & requestOpts .~ options
                            & requestParameters .~ object []
                            & requestTopic .~ "contact.created"

  options <- grabOptions
  go $ blank & requestEndpoint .~ "https://api.intercom.io/contacts"
             & requestOpts .~ options
             & requestParameters .~ object ["id" .= cid contactResp, "email" .= ("bob+{{random}}@intercom.io" :: T.Text)]
             & requestTopic .~ "contact.added_email"

  options <- grabOptions
  go $ blank & requestEndpoint .~ "https://api.intercom.io/contacts/convert"
             & requestOpts .~ options
             & requestParameters .~ object ["contact" .= object ["id" .= cid contactResp], "user" .= object ["email" .= ("bob+{{random}}@intercom.io" :: T.Text)]]
             & requestTopic .~ "contact.signed_up"

  options <- grabOptions
  go $ blank & requestEndpoint .~ "https://api.intercom.io/users"
             & requestOpts .~ options
             & requestParameters .~ object ["email" .= ("bob+{{random}}@intercom.io" :: T.Text)]
             & requestTopic .~ "user.created"

  options <- grabOptions
  go $ blank & requestEndpoint .~ "https://api.intercom.io/messages"
             & requestOpts .~ options
             & requestParameters .~ object ["from" .= object ["id" .= userId, "type" .= userType], "body" .= hi]
             & requestTopic .~ "conversation.user.created"

  options <- grabOptions
  go $ blank & requestEndpoint .~ "https://api.intercom.io/conversations/1038629832/reply"
             & requestOpts .~ options
             & requestParameters .~ object ["intercom_user_id" .= userIdForConversation, "body" .= hi, "type" .= userType, "message_type" .= commentType]
             & requestTopic .~ "conversation.user.replied"

  options <- grabOptions
  go $ blank & requestEndpoint .~ "https://api.intercom.io/conversations/1038629832/reply"
             & requestOpts .~ options
             & requestParameters .~ object ["admin_id" .= adminId, "body" .= hi, "type" .= adminType, "message_type" .= commentType]
             & requestTopic .~ "conversation.admin.replied"

  options <- grabOptions
  go $ blank & requestEndpoint .~ "https://api.intercom.io/conversations/1038629832/reply"
             & requestOpts .~ options
             & requestParameters .~ object ["admin_id" .= adminId, "body" .= hi, "type" .= adminType, "message_type" .= noteType]
             & requestTopic .~ "conversation.admin.noted"

  options <- grabOptions
  go $ blank & requestEndpoint .~ "https://api.intercom.io/conversations/1038629832/reply"
             & requestOpts .~ options
             & requestParameters .~ object ["admin_id" .= adminId, "assignee_id" .= assigneeId, "body" .= hi, "type" .= adminType, "message_type" .= assignmentType]
             & requestTopic .~ "conversation.admin.assigned"

  options <- grabOptions
  go $ blank & requestEndpoint .~ "https://api.intercom.io/tags"
             & requestOpts .~ options
             & requestParameters .~ object ["name" .= tagName, "users" .= [object ["id" .= userId]]]
             & requestTopic .~ "user.tag.created"

  options <- grabOptions
  go $ blank & requestEndpoint .~ "https://api.intercom.io/tags"
             & requestOpts .~ options
             & requestParameters .~ object ["name" .= tagName, "users" .= [object ["untag" .= True, "id" .= userId]]]
             & requestTopic .~ "user.tag.deleted"
  where go :: WebhookRequest -> StateT DefinitionListRun IO (Maybe (W.Response L.ByteString))
        go = ((^. response) <$>) . runAssertion t
        cid r = case r of
          Nothing -> mempty
          (Just resp) -> resp ^. W.responseBody . key "id" . _String
        grabOptions = lift opts

runIntercomDefinitions :: IO ()
runIntercomDefinitions = do
  info "Running Intercom definitions"
  r <- newTVarIO Nothing :: IO (TVar TopicResult)
  withAsync (server r) $ \webServer ->
    withAsync (runDefs r) $ \testRun -> wait testRun >> cancel webServer
  return ()
  where runDefs r = execStateT (run r) >=> alarm $ defaultDefinitionListRun
