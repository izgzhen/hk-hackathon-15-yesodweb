module Handler.NewsFeed where

import Import
import Feed

getNewsFeedR :: Handler Html
getNewsFeedR = do
  hn   <- appHackerNews <$> getYesod
  newses <- liftIO $ getNewses hn
  renderer <- getUrlRenderParams
  defaultLayout $ do
    setTitle "News Feed"
    $(widgetFile "newsfeed")


newsQuery (News url time title) renderer = renderer RedirectR
                                    [ ("type", "news")
                                    , ("link", url)
                                    , ("time", pack $ show time)
                                    , ("title", title)]
