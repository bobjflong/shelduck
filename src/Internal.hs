module Internal where

import           Configuration
import           Control.Concurrent
import           Control.Concurrent.STM.TVar
import           Control.Monad.STM
import           Data.Text

record :: Maybe Text -> TVar (Maybe Text) -> IO ()
record t r = atomically $ writeTVar r t

pollingIO :: Int -> TVar a -> (TVar a -> IO Bool) -> IO b -> IO (Int, b)
pollingIO c t x i = temporaryFailure >>= \f -> if f then tryAgain else finish
  where tryAgain = threadDelay pollTime >> pollingIO (c - 1) t x i
        finish = i >>= \result -> return (c, result)
        temporaryFailure = x t >>= \p -> return $ not p && c > 0
