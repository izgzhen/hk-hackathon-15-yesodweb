module Handler.Courses where

import Import


getCoursesR :: Handler Html
getCoursesR = do
  cs <- appCoursera <$> getYesod
  renderer <- getUrlRenderParams
  defaultLayout $ do
    setTitle "Courses"
    $(widgetFile "courses")
    

crsQuery (Course link title) renderer = renderer RedirectR
                                    [ ("type",  "course")
                                    , ("link",  link)
                                    , ("title", title)]
