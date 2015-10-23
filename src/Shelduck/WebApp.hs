{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Shelduck.WebApp (webAppServer) where

import           Data.Aeson.Encode.Pretty
import           Data.ByteString.Lazy     (toStrict)
import           Data.Text                (pack, splitOn)
import           Data.Text.Encoding       (decodeUtf8)
import           Shelduck.Internal
import           Shelduck.LogParser
import           Shelly
import           Yesod

data App = App

mkYesod "App" [parseRoutes| /web-ui HomeR GET |]

instance Yesod App

style = [lucius|
  body {
    background-color: #FFFFFF;
  }
  .ui.menu .item img.logo {
    margin-right: 1.5em;
  }
  .main.container {
    margin-top: 3.5em;
  }
  .ui.footer.segment {
    margin: 5em 0em 0em;
    padding: 5em 0em;
  }
  pre {
    white-space: pre-wrap;
    font-size: 10px;
    background: #f9f9f9;
  }
|]

filterKnown :: [LogLine] -> [LogLine]
filterKnown = filter (knownLog . verb)
  where knownLog (UnknownAction _) = False
        knownLog NoAction = False
        knownLog _ = True

tailLog :: IO [LogLine]
tailLog = do
  log <- logFile
  logData <- shelly $ verbosely $ run "tail" ["-100", pack log]
  return $ (filterKnown . fmap toLogLine . reverse . splitOn "\n") logData

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  logs <- liftIO tailLog
  addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/jquery/3.0.0-alpha1/jquery.js"
  addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.1.4/semantic.min.js"
  addStylesheetRemote "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.1.4/semantic.min.css"
  toWidget style
  [whamlet|
    <div class="ui fixed inverted menu">
      <div class="ui container">
        <div href="#" class="header item">
          Shelduck

    <div class="ui main text container">
      <div class="ui message">
        <div class="header">
          Log file successfully parsed
        <p>Currently viewing the last 100 log file entries</p>

      <div class="ui relaxed divided list">
        $forall log <- logs
          <div class="item">
            <div class="content">
              <a class="header">#{(show . verb) log}
              <div class="description">
                <pre>
                  $case log
                    $of Data h
                      #{(decodeUtf8 . toStrict) (encodePretty h)}
                    $of _
                      <div>
  |]
  return ()

webAppServer :: IO ()
webAppServer = warp 4567 App
