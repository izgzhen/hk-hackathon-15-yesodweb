module Watcher where

import Prelude
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar

mkWatcher :: Int -> (b -> a -> IO a) -> IO b -> a -> IO (IO a)
mkWatcher perSecond merger fetcher initial = do
    ma <- newMVar initial
    forkIO $ forever $ do
        putStrLn "Syncing...."
        a' <- fetcher
        modifyMVar_ ma $ merger a'
        putStrLn "Sync done"
        threadDelay (1000000 * perSecond)
    return $ readMVar ma
