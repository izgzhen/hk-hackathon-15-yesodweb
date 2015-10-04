module Handler.Portal where

import Import

import Yesod.Form.Bootstrap3

-- Where Personal Information is edited

getPortalR :: Handler Html
getPortalR  = do
    maid <- maybeAuthId
    case maid of
        Nothing -> redirect (AuthR LoginR)
        Just tk -> do
            pInfo <- getUserInfo tk
            sns <- (map newsWidget . catMaybes) <$> mapM (runDB . get) (userStarNews pInfo)
            scs <- (map courseWidget . catMaybes) <$> mapM (runDB . get) (userStarCourse pInfo)
            
            (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ (,)
                                    <$> aopt urlField  "Website" (Just (userWebsite pInfo))
                                    <*> aopt textField "Hobbit"  (Just (userHobbit  pInfo))
            defaultLayout $ do            
                setTitle "Portal"
                $(widgetFile "portal")

postPortalR :: Handler Html
postPortalR = do
    maid <- maybeAuthId
    case maid of
        Nothing -> redirect (AuthR LoginR)
        Just tk -> do
            ((result, widget), enctype) <- runFormPost $ renderDivs $ (,)
                                                      <$> aopt urlField  "Website" Nothing
                                                      <*> aopt textField "Hobbit"  Nothing
            case result of
              FormSuccess (website, hobbit) -> do
                uid <- getUserId tk
                runDB $ update uid [UserWebsite =. website, UserHobbit =. hobbit]
                redirect PortalR
              _ -> defaultLayout
                       [whamlet|
                           <p>Invalid input, let's try again.
                           <a href=@{PortalR}> Go back to portal
                       |]

getUserInfo :: Text -> Handler User
getUserInfo tk = do
    Just (Entity _ user) <- runDB $ getBy (UniqueGid tk)
    return user

getUserId tk = do
    Just (Entity uid _) <- runDB $ getBy (UniqueGid tk)
    return uid


courseWidget :: Course -> Widget
courseWidget (Course link title) =
    [whamlet|
        <p>
          <a href=#{link}>#{title}
    |]

newsWidget :: News -> Widget
newsWidget (News link time title) =
    [whamlet|
      <p>
        <a href=#{link}>#{title}
        <small>published in #{show time}
    |]
