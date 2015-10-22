{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Shelduck.WebApp where

import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.HashMap.Strict
import           Shelduck.LogParser
import           System.Directory
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
|]

tailLog :: IO [BL.ByteString]
tailLog = do
  home <- getHomeDirectory
  file <- BL.readFile (mconcat [home, "/shelduck.log"])
  return $ (take 100 . reverse . BL.split '\n') file

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  logData <- liftIO tailLog
  let logs = fmap toLogLine logData
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
          $case (verb log)
             $of UnknownAction _
               <div>
             $of NoAction
               <div>
             $of _
               <div class="item">
                 <div class="content">
                   <a class="header">#{(show . verb) log}
                   <div class="description">
                     <pre>
                       $case log
                         $of Data h
                           #{(show . toList) h}
                         $of _
                           <div>
  |]
  return ()

server :: IO ()
server = warp 4567 App
