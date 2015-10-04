module Handler.Redirect where

import Import
import qualified Data.Map as M
import Prelude (read)

getRedirectR :: Handler Html
getRedirectR = do
    req <- getRequest
    let params = M.fromList (reqGetParams req)
    case (M.lookup "type" params, M.lookup "link" params) of
        (Just "news", Just url) -> updateUserInfo params
                                                  url
                                                  newsRetriever
                                                  (\nid u -> [UserStarNews =. (nid : userStarNews u)])
        (Just "course", Just url) -> updateUserInfo params
                                                    url
                                                    (\_ _ -> 
                                                        let Just title = M.lookup "title" params
                                                        in  Course url title)
                                                    (\cid u -> [UserStarCourse =. (cid : userStarCourse u)])
        _ -> do
            setMessage "params type not supported"
            redirect HomeR
    where
        updateUserInfo params url retriever updator = do
            maid <- maybeAuthId
            case maid of
                Nothing -> redirect url
                Just tk -> do
                    uid   <- getUserId tk
                    newId <- runDB $ insert (retriever url params)
                    mu    <- runDB $ get uid
                    case (mu :: Maybe User) of
                        (Just u) -> runDB $ update uid (updator newId u)
                        Nothing  -> return ()
                    redirect url

        newsRetriever url params =
            let (Just title, Just time) = (M.lookup "title" params, M.lookup "time" params)
                utcTime = read (unpack time) :: UTCTime
            in  News url utcTime title


getUserId tk = do
    Just (Entity uid _) <- runDB $ getBy (UniqueGid tk)
    return uid
