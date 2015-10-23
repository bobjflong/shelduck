module Main (
  main
)  where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Shelduck.IntercomDefinitions
import           Shelduck.WebApp
import           System.Environment

printUsage :: IO ()
printUsage = mapM_ putStrLn usageLines
  where usageLines = ["USAGE: shelduck <definition_type>", "Current definition_types: intercom"]

runIntercomDefinitionsFromCommandLine :: [String] -> IO ()
runIntercomDefinitionsFromCommandLine intercomArgs =
  case intercomArgs of
    ("loop":_) -> runIntercomDefinitions >> threadDelay 1800000000 >> runIntercomDefinitionsFromCommandLine intercomArgs
    _ -> runIntercomDefinitions

main = do
  args <- getArgs
  withAsync webAppServer $ \app -> do
    case args of
      ("intercom":args) -> runIntercomDefinitionsFromCommandLine args
      _ -> printUsage
    wait app
