module Main (
  main
)  where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import           Shelduck.IntercomDefinitions
import           Shelduck.Internal
import           Shelduck.WebApp
import           System.Environment
import           System.IO
import           System.Posix.Process.ByteString

createLogFile :: IO ()
createLogFile = logFile >>= openAndClose
  where openAndClose = flip openFile AppendMode >=> hClose

printUsage :: IO ()
printUsage = mapM_ putStrLn usageLines
  where usageLines = ["USAGE: shelduck <definition_type>", "Current definition_types: intercom"]

runIntercomDefinitionsFromCommandLine :: [String] -> IO ()
runIntercomDefinitionsFromCommandLine intercomArgs =
  case intercomArgs of
    ("loop":_) -> runIntercomDefinitions >> threadDelay 1800000000 >> runIntercomDefinitionsFromCommandLine intercomArgs
    _ -> runIntercomDefinitions

writePid :: IO ()
writePid = getProcessID >>= writeProcessID
  where writeProcessID = writeFile "shelduck.pid" . show

main = do
  createLogFile
  writePid
  args <- getArgs
  withAsync webAppServer $ \app -> do
    case args of
      ("intercom":args) -> runIntercomDefinitionsFromCommandLine args
      _ -> printUsage
    wait app
