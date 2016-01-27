{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Agon.Server(app) where

import Agon.Agon
import Agon.Types
import Agon.Instances
import Agon.APIConstructors
import Agon.API
import Agon.Auth
import Agon.ServerConstructors
import Agon.FBAuth
import Agon.Data
import Agon.Data.Types
import Agon.Data.Instances
import Agon.HTTP

import Control.Lens ((&), (^.), (%~), (.~), over, view)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Data.Char (toLower)
import Data.List (find)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Network.Wai
import Servant

type AppAPI = "api" :> AgonAPI

app :: Agon -> Application
app = serve (Proxy :: Proxy AppAPI) . agonServer

agonServer :: Agon -> Server AppAPI
agonServer agon = enter (agonToServant agon) server

agonToServant' :: Agon -> AppM a -> ServantM a
agonToServant' agon act = runAgonM act agon

agonToServant :: Agon -> AppM :~> ServantM
agonToServant agon = Nat $ agonToServant' agon

server :: ServerT AgonAPI AppM
server = authServer :<|> usersServer :<|> teamsServer :<|> eventsServer :<|> workoutsServer :<|> entrantsServer

authServer :: ServerT AuthAPI AppM
authServer = authenticate

usersServer :: ServerT UsersAPI AppM
usersServer = readServer "users" publicAccess

teamsServer :: ServerT TeamsAPI AppM
teamsServer = readServer "teams" publicAccess :<|> createTeam :<|> inviteToTeam :<|> acceptTeamInvite :<|> removeFromTeam

eventsServer :: ServerT EventsAPI AppM
eventsServer = readServer "events" eventAccess :<|> createUpdateServer adminAccess :<|> deleteServer adminAccess

workoutsServer :: ServerT WorkoutsAPI AppM -- Should theoretically build an access predicate that checks event visibility, but dont think this will get exploited
workoutsServer = readServerDependent "workouts" publicAccess :<|> createUpdateServer adminAccess :<|> deleteServer adminAccess

entrantsServer :: ServerT EntrantsAPI AppM
entrantsServer = readServerDependent "entrants" publicAccess :<|> createServer entrantCreateAccess :<|> setScore

---
--- Auth API
---

authenticate :: String -> AppM LoginInfo
authenticate tok = do
    (usr, p) <- login tok
    tok <- encrypt p
    return $ LoginInfo usr tok

---
--- Teams API
---

createTeam :: AuthToken -> String -> AppM Team
createTeam tok name = withAuth tok $ \usr -> do
    let uname = "\"" ++ fmap toLower name ++ "\""
    eurl <- viewA agonCouchDBURL >>= return . (++ "_design/agon/_view/teamsByName")
    tx <- (list eurl (Just uname) :: AppM [Team])
    case (length tx) of
        0 -> do
            uuid <- fmap head (uuids 1)
            let t = Team Nothing uuid name [AdminMember $ usr ^. userID] []
            update t
        _ -> lift . left $ err409 { errBody = "Team name is not available" }

inviteToTeam :: AuthToken -> ID -> ID -> AppM Team
inviteToTeam tok tid uid = withAuth tok $ \executor -> do
    team <- get tid
    invitee <- get uid
    let isTA = isTeamAdmin executor team
    let isMember = isTeamMember invitee team
    let invited = hasInvite invitee team
    case (isTA && not isMember && not invited) of
        True -> do
            let team' = team & teamInvitations %~ (++[invitee ^. userID])
            update team'
        False -> lift $ left err
            where
            err | not isTA = err403
                | isMember = err400 { errBody = "Invitee is already a member" }
                | otherwise = err400 { errBody = "Invitee has a pending invitation" }

acceptTeamInvite :: AuthToken -> ID -> AppM Team
acceptTeamInvite tok tid = withAuth tok $ \usr -> do
    let uid = usr ^. userID
    team <- get tid
    let invited = hasInvite usr team
    case invited of
        True -> do
            let team' = team & teamMembers %~ (++[Member uid]) & teamInvitations %~ (filter (/=uid))
            update team'
        False -> lift . left $ err400 { errBody = "No such invitation exists" }

removeFromTeam :: AuthToken -> ID -> ID -> AppM Team
removeFromTeam tok tid uid = withAuth tok $ \executor -> do
    team <- get tid
    removed <- get uid
    let executorID = executor ^. userID
    let removedID = removed ^. userID
    let isTA = isTeamAdmin executor team
    let removeSelf = executorID == removedID
    case (isTA && not removeSelf) of
        True -> do
            let team' = team & teamMembers %~ (filter $ (/=removedID) . memberID)
            update team'
        False -> lift $ left err
            where
            err | not isTA = err403
                | otherwise = err400 { errBody = "Cannot remove self" }

isTeamAdmin :: User -> Team -> Bool
isTeamAdmin u = isJust . find (==ta) . view teamMembers
    where ta = AdminMember (u ^. userID)

isTeamMember :: User -> Team -> Bool
isTeamMember u = isJust . find ((==uid) . memberID) . view teamMembers
    where uid = u ^. userID

hasInvite :: User -> Team -> Bool
hasInvite u = isJust . find (==uid) . view teamInvitations
    where uid = u ^. userID

memberID :: TeamMember -> ID
memberID (AdminMember uid) = uid
memberID (Member uid) = uid

---
--- Entrants API
---

setScore :: AuthToken -> ID -> Score -> AppM Entrant
setScore tok eid score = withAuthAdmin tok $ \usr -> do
    entrant <- get eid
    let entEventID = entrant ^. entrantEventID
    scoreWorkout <- get (score ^. scoreWorkoutID)
    let scoreEventID = scoreWorkout ^. workoutEventID
    case (scoreEventID == entEventID) of
        True -> do
            let workID = scoreWorkout ^. workoutID
            let escores = filter ((/=workID) . view scoreWorkoutID) $ entrant ^. entrantScores
            let escores' = escores ++ [score & scoreType .~ (scoreWorkout ^. workoutScoreType)]
            let entrant' = entrant & entrantScores .~ escores'
            update entrant'
        False -> lift $ left err400

---
--- Access Predicates
---

entrantCreateAccess :: AccessPredicate Entrant
entrantCreateAccess Nothing _ = return False
entrantCreateAccess (Just usr) entrant = do
    let ekey = "\"" ++ (entrant ^. entrantEventID) ++ "\""
    eurl <- viewA agonCouchDBURL >>= return . (++ "_design/agon/_view/entrants")
    existingEntrants <- (list eurl (Just ekey) :: AppM [Entrant])
    event <- get (entrant ^. entrantEventID)
    let ent = entrant ^. entrantEntity
    let maxed = (event ^. eventMaxEntrants) <= (length $ filter _entrantConfirmed existingEntrants)
    let entered = isJust $ find ((==ent) . view entrantEntity) existingEntrants
    let correctType = isCorrectEntityType ent event
    let eligable = isEligableToEnter usr event
    return (not maxed && not entered && correctType && eligable)

eventAccess :: AccessPredicate Event
eventAccess Nothing e = return (e ^. eventVisible)
eventAccess (Just usr) e = return (usr ^. userIsAdmin || e ^. eventVisible)

isEligableToEnter :: User -> Event -> Bool
isEligableToEnter u e = (u ^. userIsAdmin) || (view eventVisible e && not (view eventIsInvitational e))

isCorrectEntityType :: EntrantEntity -> Event -> Bool
isCorrectEntityType (IndividualEntrant _) = isIndividualEvent
isCorrectEntityType (TeamEntrant _) = not . isIndividualEvent

isIndividualEvent :: Event -> Bool
isIndividualEvent e = ii (e ^. eventType)
    where
    ii IndividualEvent = True
    ii _ = False

---
---
---

notImplemented :: AppM a
notImplemented = lift . left $ err500 { errBody = "Not Implemented" }
