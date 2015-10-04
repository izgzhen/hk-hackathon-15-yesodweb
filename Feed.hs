-- Feed
{-# LANGUAGE DeriveGeneric #-}
module Feed where

import Prelude
import GHC.Generics
import Data.Aeson as A
import Control.Concurrent (forkIO, threadDelay)
import Network.HTTP.Conduit as H
import Prelude (read)
import qualified Data.HashMap.Strict as M
import Data.Maybe
import Data.Time.Clock
import qualified Data.ByteString.Lazy.Char8 as C
import Control.Monad (forever,forM)
import Model
import Watcher

data HackerNews = HackerNews {
    getNewses :: IO [News]
}


mkHackerNews :: Int -> IO HackerNews
mkHackerNews perSecond = do
    returner <- mkWatcher perSecond merger fetcher M.empty

    return $ HackerNews {
            getNewses = M.elems <$> returner
        }

    where
        fetcher :: IO [Int]
        fetcher = do
            res <- simpleHttp "https://hacker-news.firebaseio.com/v0/topstories.json"
            return (take 10 $ read (C.unpack res) :: [Int])

        merger res' p = do
            l <- forM res' $ \i -> 
                case M.lookup i p of
                    Just n  -> return $ Just (i, n)
                    Nothing -> do
                        mn <- getNews i
                        case mn of
                            Nothing -> return Nothing
                            Just n  -> return $ Just (i, n)
            return (M.fromList $ map fromJust $ filter (/= Nothing) l)


getNews :: Int -> IO (Maybe News)
getNews i = do
    let url = "https://hacker-news.firebaseio.com/v0/item/" ++ show i ++ ".json"
    res <- simpleHttp url
    let res' = decode res :: Maybe Value
    case res' of
        Nothing -> return Nothing
        Just (Object m) -> do
            now <- getCurrentTime
            let (A.String urlStr) = fromJust $ M.lookup "url" m
            let (A.String ttlStr) = fromJust $ M.lookup "title" m
            let news = News urlStr now ttlStr
            return (Just news)
