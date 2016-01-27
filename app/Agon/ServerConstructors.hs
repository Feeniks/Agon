{-# LANGUAGE TypeFamilies #-}

module Agon.ServerConstructors(
    AccessPredicate,
    readServer,
    readServerDependent,
    listServer,
    listServerDependent,
    getServer,
    createUpdateServer,
    createServer,
    updateServer,
    deleteServer,
    publicAccess,
    userAccess,
    adminAccess
) where

import Agon.Types
import Agon.Instances
import Agon.Agon
import Agon.APIConstructors
import Agon.Data
import Agon.Auth

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Data.Aeson (FromJSON, ToJSON)
import Servant

type AccessPredicate e = Maybe User -> e -> AppM Bool --TODO: if performance becomes a thing, it may be a good idea to change this to compile to a query of some kind

type EntityView = String
type ViewKey = String

readServer :: (Entity e, FromJSON e) => EntityView -> AccessPredicate e -> ServerT (ReadAPI e) AppM
readServer ev pred = listServer ev pred :<|> getServer pred

readServerDependent :: (Entity e, FromJSON e) => EntityView -> AccessPredicate e -> ServerT (ReadDependentAPI e) AppM
readServerDependent ev pred = listServerDependent ev pred :<|> getServer pred

createUpdateServer :: (Entity e, ToJSON e) => AccessPredicate e -> ServerT (CreateUpdateAPI e) AppM
createUpdateServer pred = createServer pred :<|> updateServer pred

listServer :: (Entity e, FromJSON e) => EntityView -> AccessPredicate e -> AuthToken -> AppM [e]
listServer ev pred tok = withAuthOpt tok $ \musr -> do
    rx <- list ev Nothing
    filterM (pred musr) rx

listServerDependent :: (Entity e, FromJSON e) => EntityView -> AccessPredicate e -> AuthToken -> ID -> AppM [e]
listServerDependent ev pred tok depID = withAuthOpt tok $ \musr -> do
    rx <- list ev (Just depID)
    filterM (pred musr) rx

getServer :: (Entity e, FromJSON e) => AccessPredicate e -> AuthToken -> ID -> AppM e
getServer pred tok eid = withAuthOpt tok $ \musr -> do
    r <- get eid
    access <- pred musr r
    case access of
        True -> return r
        False -> lift . left $ err403

createServer :: (Entity e, ToJSON e) => AccessPredicate e -> AuthToken -> e -> AppM e
createServer pred tok e = withAuthOpt tok $ \musr -> do
    access <- pred musr e
    case access of
        True -> create e
        False -> lift . left $ err403

updateServer :: (Entity e, ToJSON e) => AccessPredicate e -> AuthToken -> e -> AppM e
updateServer pred tok e = withAuthOpt tok $ \musr -> do
    access <- pred musr e
    case access of
        True -> update e
        False -> lift . left $ err403

deleteServer :: AccessPredicate () -> AuthToken -> ID -> ID -> AppM ()
deleteServer pred tok eid rev = withAuthOpt tok $ \musr -> do
    access <- pred musr ()
    case access of
        True -> delete eid rev
        False -> lift . left $ err403

publicAccess :: AccessPredicate e
publicAccess _ _ = return True

userAccess :: AccessPredicate e
userAccess Nothing _ = return False
userAccess (Just _) _ = return True

adminAccess :: AccessPredicate e
adminAccess Nothing _ = return False
adminAccess (Just usr) _ = return (_userIsAdmin usr)
