{-# LANGUAGE OverloadedStrings #-}

module Agon.Data(
    uuids,
    list,
    get,
    create,
    update,
    delete
) where

import Agon.Types
import Agon.Instances
import Agon.Agon
import Agon.APIConstructors
import Agon.Data.Types
import Agon.Data.Instances
import Agon.HTTP

import Control.Lens ((&), (.~), set, view)
import Data.Aeson
import Servant

---
--- Generalised accessors
---

uuids :: Int -> AppM [String]
uuids n = do
    url <- viewA agonCouchBaseURL >>= return . (++ ("_uuids?count=" ++ show n))
    req <- buildReq url "GET" noBody
    httpJSON req >>= return . unUUIDs
    where unUUIDs (UUIDs sx) = sx

list :: (Entity e, FromJSON e) => String -> Maybe String -> AppM [e]
list tag mkey = do
    url <- viewURL tag mkey
    req <- buildReq url "GET" noBody
    fmap (fmap cliItem . clItems) $ httpJSON req

get :: (Entity e, FromJSON e) => String -> AppM e
get eid = do
    url <- docURL eid
    req <- buildReq url "GET" noBody
    httpJSON req

create :: (Entity e, ToJSON e) => e -> AppM e
create e = do
    uuid <- fmap head $ uuids 1
    let e' = set (entityID e) uuid e
    update e'

update :: (Entity e, ToJSON e) => e -> AppM e
update e = do
    url <- docURL (view (entityID e) e)
    req <- buildReq url "PUT" (Just e)
    res <- httpJSON req
    return $ e & (entityRev e) .~ (Just $ curRev res)

delete :: String -> String -> AppM ()
delete eid rev = do
    url <- viewA agonCouchDBURL >>= return . (++ (eid ++ "?rev=" ++ rev))
    req <- buildReq url "DELETE" noBody
    http' req

---
---
---

viewURL :: String -> Maybe String -> AppM String
viewURL tag mkey = viewA agonCouchDBURL >>= return . (++ ("_design/agon/_view/" ++ tag ++ keyStr))
    where
    keyStr = case mkey of
        Nothing -> ""
        (Just k) -> "?key=" ++ k

docURL :: String -> AppM String
docURL eid = viewA agonCouchDBURL >>= return . (++ eid)

noBody :: Maybe ()
noBody = Nothing
