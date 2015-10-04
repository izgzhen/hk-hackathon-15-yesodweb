module Handler.Events where

import Import
import Event

getEventsR :: Handler Html
getEventsR = do
    esM  <- appEvents <$> getYesod
    es   <- liftIO esM
    defaultLayout $ do
      setTitle "Events"
      $(widgetFile "events")
