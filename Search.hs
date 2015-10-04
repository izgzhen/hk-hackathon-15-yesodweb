module Search where

import Model
import Prelude
import Network.HTTP.Conduit as H
import qualified Data.HashMap.Strict as M
import Data.Aeson as A
import Data.Maybe
import qualified Data.Vector as V
import Data.Text (append)
-- Course Search

-- The category API: ""

-- The search API: "https://api.coursera.org/api/catalog.v1/courses?q=search&query=malware+underground"

-- This is an example of VERY persistent data ... 
mkCourseraCategory :: IO [Course]

mkCourseraCategory = do
    let url = "https://api.coursera.org/api/catalog.v1/categories?id=11&includes=courses"
    res <- simpleHttp url
    let mobj = decode res :: Maybe Object
    let mvec = do
        m <- mobj
        (Object m') <- M.lookup "linked" m
        (Array vec) <- M.lookup "courses" m'
        return vec
    case mvec of
        Just vec -> return $ V.toList (V.map transform vec)
        Nothing  -> return []
    where
        transform :: Value -> Course
        transform (Object m) = Course link title
            where
                A.String linkStr = fromJust $ M.lookup "shortName" m
                link  = "https://www.coursera.org/course/" `append` linkStr
                A.String title = (fromJust $ M.lookup "name" m)
	transform _ = error "impossible happens!"


