{-# LANGUAGE OverloadedStrings #-}

module IntercomDefinitions where

import           Control.Concurrent.Async
import           Control.Concurrent.STM.TVar
import           Control.Lens                hiding ((.=))
import           Control.Monad.STM
import           Control.Monad.Trans.Reader
import           Data.Aeson                  (object, (.=))
import           Data.ByteString             hiding (getLine, pack)
import           Data.ByteString.Char8       (pack)
import qualified Data.Text                   as T
import qualified Network.Wreq                as W
import           Scalpel                     hiding (opts)
import           System.Environment

opts = do
  appId <- getEnv "SCALPEL_INTERCOM_APP_ID"
  appApiKey <- getEnv "SCALPEL_INTERCOM_APP_API_KEY"
  return $ W.defaults & W.header "Accept" .~ ["application/json"]
                      & W.header "Content-Type" .~ ["application/json"]
                      & W.auth ?~ W.basicAuth (pack appId) (pack appApiKey)

type DefinitionList = IO [WebhookRequest]

--
-- Example parameters for API requests
--
userId :: T.Text
userId = "55b26822ce97179e52001334"

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

intercomDefinitions :: DefinitionList
intercomDefinitions = do
  options <- opts
  return [
    blank & requestEndpoint .~ "https://api.intercom.io/contacts"
          & requestOpts .~ options
          & requestParameters .~ object []
          & requestTopic .~ "contact.created"
    ,
    blank & requestEndpoint .~ "https://api.intercom.io/contacts"
          & requestOpts .~ options
          & requestParameters .~ object ["id" .= contactId, "email" .= ("bob+{{random}}@intercom.io" :: T.Text)]
          & requestTopic .~ "contact.added_email"
    ,
    blank & requestEndpoint .~ "https://api.intercom.io/contacts/convert"
          & requestOpts .~ options
          & requestParameters .~ object ["contact" .= object ["id" .= contactId], "user" .= object ["email" .= ("bob+{{random}}@intercom.io" :: T.Text)]]
          & requestTopic .~ "contact.signed_up"
    ,
    blank & requestEndpoint .~ "https://api.intercom.io/users"
          & requestOpts .~ options
          & requestParameters .~ object ["email" .= ("bob+{{random}}@intercom.io" :: T.Text)]
          & requestTopic .~ "user.created"
    ,
    blank & requestEndpoint .~ "https://api.intercom.io/messages"
          & requestOpts .~ options
          & requestParameters .~ object ["from" .= object ["id" .= userId, "type" .= userType], "body" .= hi]
          & requestTopic .~ "conversation.user.created"
    ,
    blank & requestEndpoint .~ "https://api.intercom.io/conversations/1039067180/reply"
          & requestOpts .~ options
          & requestParameters .~ object ["intercom_user_id" .= userId, "body" .= hi, "type" .= userType, "message_type" .= commentType]
          & requestTopic .~ "conversation.user.replied"
    ,
    blank & requestEndpoint .~ "https://api.intercom.io/conversations/1039067180/reply"
          & requestOpts .~ options
          & requestParameters .~ object ["admin_id" .= adminId, "body" .= hi, "type" .= adminType, "message_type" .= commentType]
          & requestTopic .~ "conversation.admin.replied"
    ,
    blank & requestEndpoint .~ "https://api.intercom.io/conversations/1039067180/reply"
          & requestOpts .~ options
          & requestParameters .~ object ["admin_id" .= adminId, "body" .= hi, "type" .= adminType, "message_type" .= noteType]
          & requestTopic .~ "conversation.admin.noted"
    ,
    blank & requestEndpoint .~ "https://api.intercom.io/conversations/1039067180/reply"
          & requestOpts .~ options
          & requestParameters .~ object ["admin_id" .= adminId, "assignee_id" .= assigneeId, "body" .= hi, "type" .= adminType, "message_type" .= assignmentType]
          & requestTopic .~ "conversation.admin.assigned"
    ,
    blank & requestEndpoint .~ "https://api.intercom.io/tags"
          & requestOpts .~ options
          & requestParameters .~ object ["name" .= tagName, "users" .= [object ["id" .= userId]]]
          & requestTopic .~ "user.tag.created"
    ,
    blank & requestEndpoint .~ "https://api.intercom.io/tags"
          & requestOpts .~ options
          & requestParameters .~ object ["name" .= tagName, "users" .= [object ["untag" .= True, "id" .= userId]]]
          & requestTopic .~ "user.tag.deleted"
    ]

runIntercomDefinitions :: IO ()
runIntercomDefinitions = do
  info "Running Intercom definitions"
  r <- newTVarIO Nothing :: IO (TVar TopicResult)
  definitions <- intercomDefinitions
  concurrently (server r) (mapM_ (runDefiniton r) definitions)
  getLine
  return ()

runDefiniton :: TVar TopicResult -> WebhookRequest -> IO Bool
runDefiniton t w = let req = performRequest w in runReaderT req t
