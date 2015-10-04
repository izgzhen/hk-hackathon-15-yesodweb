module Event where

import Watcher
import Data.Text (Text)
import Yesod (Html)
import Prelude
import Network.HTTP.Conduit as H
import qualified Data.HashMap.Strict as M
import Data.Aeson as A
import Data.Maybe
import qualified Data.Vector as V
import Data.Text (append)
import Text.Blaze.Html

-- Interfacing with the Meetup.com
-- Hold code event with special fields
-- Fields that I can come up with
-- * GitHub Repo
-- * Materials (by OSS)
-- * Specialization (FPL, OOP etc.)

-- Tech: 34


demoURL = "https://api.meetup.com/2/open_events?and_text=False&offset=0&format=json&lon=114.13999939&limited_events=False&photo-host=public&page=20&radius=25.0&category=34&lat=22.2700004578&desc=False&status=upcoming&sig_id=191898068&sig=645d946e657df11db43d4d1e3afa45814d87f213"


data Event = Event {
    name        :: Text,
    description :: Html,
    url         :: Text,
    group       :: Text,
    status      :: Status
}

data Status = Past
            | Upcoming
            | Cancelled
            | Proposed
            | Suggested
            | Draft
            deriving (Show)

watchEvent = mkWatcher 100 merger fetcher []
    where
        merger es _ = return es

        fetcher :: IO [Event]
        fetcher = do
            res <- simpleHttp demoURL
            let mobj = decode res :: Maybe Object
            let ret = do
                m <- mobj
                (Array vec) <- M.lookup "results" m
                return (catMaybes $ V.toList (V.map transform vec))
            return $ case ret of
                Nothing -> []
                Just xs -> xs

        transform (Object m) = Event <$> getField "name"
                                     <*> (fmap preEscapedToMarkup (getField "description"))
                                     <*> getField "event_url"
                                     <*> getGroupField "name"
                                     <*> (getField "status" >>= readStatus)
            where
                getField s = (\(A.String s) -> s) <$> M.lookup s m
                getGroupField s = do
                    (Object gm) <- M.lookup "group" m
                    (\(A.String s) -> s) <$> M.lookup s gm

                readStatus "upcoming"   = Just Upcoming
                readStatus "cancelled"  = Just Cancelled
                readStatus "upcoming"   = Just Upcoming
                readStatus "past"       = Just Past
                readStatus "proposed"   = Just Proposed
                readStatus "suggested"  = Just Suggested
                readStatus "draft"      = Just Draft
                readStatus _            = Nothing



