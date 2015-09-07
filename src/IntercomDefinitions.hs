{-# LANGUAGE OverloadedStrings #-}

module IntercomDefinitions where

import           Control.Lens          hiding ((.=))
import           Data.ByteString       hiding (pack, getLine)
import           Data.ByteString.Char8 (pack)
import qualified Network.Wreq          as W
import           System.Environment
import Scalpel hiding (opts)
import Data.Aeson ((.=), object)
import qualified Data.Text as T
import           Control.Concurrent.STM.TVar
import           Control.Monad.STM
import           Control.Concurrent.Async
import           Control.Monad.Trans.Reader

opts = do
  appId <- getEnv "SCALPEL_INTERCOM_APP_ID"
  appApiKey <- getEnv "SCALPEL_INTERCOM_APP_API_KEY"
  return $ W.defaults & W.header "Accept" .~ ["application/json"]
                      & W.header "Content-Type" .~ ["application/json"]
                      & W.auth ?~ W.basicAuth (pack appId) (pack appApiKey)

type DefinitionList = IO [WebhookRequest]

intercomDefinitions :: DefinitionList
intercomDefinitions = do
  options <- opts
  return [
    blank & requestEndpoint .~ "https://api.intercom.io/users"
          & requestOpts .~ options
          & requestParameters .~ object ["email" .= ("bob+{{random}}@intercom.io" :: T.Text)]
          & requestTopic .~ "user.created"
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
