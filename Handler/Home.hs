module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
    maid <- maybeAuthId
    defaultLayout $ do
        setTitle "Home"
        $(widgetFile "homepage")

