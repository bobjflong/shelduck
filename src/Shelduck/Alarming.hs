{-# LANGUAGE OverloadedStrings #-}

module Shelduck.Alarming (
  shouldAlarm,
  alarm
)  where

import           Control.Lens           hiding ((.=))
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Functor
import qualified Data.Text              as T
import           Shelduck
import           Shelduck.Configuration
import           Shelduck.Slack

shouldAlarm :: DefinitionListRun -> Bool
shouldAlarm d = ((failed / total) :: Double) >= alarmThreshold
  where failed = fromIntegral $ d ^. assertionFailedCount
        total = fromIntegral $ d ^. assertionCount

alarm :: (MonadIO m) => DefinitionListRun -> m ()
alarm r = if shouldAlarm r then void (liftIO doAlarm) else noop
  where noop = return ()
        doAlarm = sendToSlackRaw $ object ["text" .= alertText]
        alertText :: T.Text
        alertText = "*Alert: test failures have breached threshold*"
