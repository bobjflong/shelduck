module Main (
  main
)  where

import           Shelduck.IntercomDefinitions
import           System.Environment

printUsage :: IO ()
printUsage = mapM_ putStrLn usageLines
  where usageLines = ["USAGE: shelduck <definition_type>", "Current definition_types: intercom"]

main = do
  args <- getArgs
  case args of
    ("intercom":_) -> runIntercomDefinitions
    _ -> printUsage
