{-# LANGUAGE OverloadedStrings #-}

module Agon.Instances where

import Agon.Types

import Control.Applicative
import Control.Lens (view)
import Control.Monad
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Maybe (maybeToList)
import qualified Data.Text as T

instance Entity User where
    entityRev _ = userRev
    entityID _ = userID

instance Entity Team where
    entityRev _ = teamRev
    entityID _ = teamID

instance Entity Event where
    entityRev _ = eventRev
    entityID _ = eventID

instance Entity Workout where
    entityRev _ = workoutRev
    entityID _ = workoutID

instance Entity Entrant where
    entityRev _ = entrantRev
    entityID _ = entrantID

instance ToJSON Principal where
    toJSON (Principal uid exp) = object ["uid" .= uid, "exp" .= exp]

instance FromJSON Principal where
    parseJSON (Object v) = Principal <$> v .: "uid" <*> v .: "exp"
    parseJSON _ = mzero

instance ToJSON LoginInfo where
    toJSON (LoginInfo u t) = object ["user" .= u, "token" .= t]

instance ToJSON AgonResult where
    toJSON (AgonResult dta) = object ["data" .= dta]

instance ToJSON User where
    toJSON u = object $ encodeRev u ++ ["type" .= ("user" :: String), "_id" .= _userID u, "name" .= _userName u, "isAdmin" .= _userIsAdmin u]

instance FromJSON User where
    parseJSON (Object v) = User <$> v .:? "_rev" <*> v .: "_id" <*> v .: "name" <*> v .: "isAdmin"
    parseJSON _ = mzero

instance ToJSON Team where
    toJSON t = object $ encodeRev t ++ ["type" .= ("team" :: String), "_id" .= _teamID t, "name" .= _teamName t, "members" .= _teamMembers t, "invitations" .= _teamInvitations t]

instance FromJSON Team where
    parseJSON (Object v) = Team <$> v .:? "_rev" <*> v .: "_id" <*> v .: "name" <*> v .: "members" <*> v .: "invitations"
    parseJSON _ = mzero

instance ToJSON TeamMember where
    toJSON (AdminMember uid) = object ["userID" .= uid, "isAdmin" .= True]
    toJSON (Member uid) = object ["userID" .= uid, "isAdmin" .= False]

instance FromJSON TeamMember where
    parseJSON (Object v) = do
        isAdmin <- v .: "isAdmin"
        let cons | isAdmin = AdminMember
                 | otherwise = Member
        cons <$> v .: "userID"
    parseJSON _ = mzero

instance ToJSON Event where
    toJSON e =
        object $ encodeRev e ++ [
            "type" .= ("event" :: String),
            "_id" .= _eventID e,
            "venueName" .= _eventVenueName e,
            "venueLatitude" .= _eventVenueLatitude e,
            "venueLongitude" .= _eventVenueLongitude e,
            "visible" .= _eventVisible e,
            "startTimeUTC" .= _eventStartTimeUTC e,
            "endTimeUTC" .= _eventEndTimeUTC e,
            "entryType" .= _eventType e,
            "entryFee" .= _eventEntryFee e,
            "isInvitational" .= _eventIsInvitational e,
            "maxEntrants" .= _eventMaxEntrants e,
            "name" .= _eventName e,
            "description" .= _eventDescription e
        ]

instance FromJSON Event where
    parseJSON (Object v) =
        Event   <$> v .:? "_rev"
                <*> v .: "_id"
                <*> v .: "venueName"
                <*> v .: "venueLatitude"
                <*> v .: "venueLongitude"
                <*> v .: "visible"
                <*> v .: "startTimeUTC"
                <*> v .: "endTimeUTC"
                <*> v .: "entryType"
                <*> v .: "entryFee"
                <*> v .: "isInvitational"
                <*> v .: "maxEntrants"
                <*> v .: "name"
                <*> v .: "description"
    parseJSON _ = mzero

instance ToJSON EventType where
    toJSON IndividualEvent = object ["type" .= ("individual" :: String)]
    toJSON (TeamEvent min) = object ["type" .= ("team" :: String), "minTeamSize" .= min]

instance FromJSON EventType where
    parseJSON (Object v) = do
        typ <- (v .: "type" :: Parser String)
        case typ of
            "individual" -> pure IndividualEvent
            "team" -> TeamEvent <$> v .: "minTeamSize"
            otherwise -> mzero
    parseJSON _ = mzero

instance ToJSON Workout where
    toJSON w = object $ encodeRev w ++ ["type" .= ("workout" :: String), "_id" .= _workoutID w, "eventID" .= _workoutEventID w, "scoreType" .= _workoutScoreType w, "timeUTC" .= _workoutTimeUTC w, "name" .= _workoutName w, "description" .= _workoutDescription w]

instance FromJSON Workout where
    parseJSON (Object v) = Workout <$> v .:? "_rev" <*> v .: "_id" <*> v .: "eventID" <*> v .: "scoreType" <*> v .: "timeUTC" <*> v .: "name" <*> v .: "description"
    parseJSON _ = mzero

instance ToJSON ScoreType where
    toJSON ScoreTime = String "time"
    toJSON ScoreReps = String "reps"
    toJSON ScoreWeight = String "weight"

instance FromJSON ScoreType where
    parseJSON (String v)
        | vs == "time" = pure ScoreTime
        | vs == "reps" = pure ScoreReps
        | vs == "weight" = pure ScoreWeight
        | otherwise = mzero
        where vs = T.unpack v
    parseJSON _ = mzero

instance ToJSON Entrant where
    toJSON e = object $ encodeRev e ++ ["type" .= ("entrant" :: String), "_id" .= _entrantID e, "eventID" .= _entrantEventID e, "entity" .= _entrantEntity e, "confirmed" .= _entrantConfirmed e, "scores" .= _entrantScores e]

instance FromJSON Entrant where
    parseJSON (Object v) = Entrant <$> v .:? "_rev" <*> v .: "_id" <*> v .: "eventID" <*> v .: "entity" <*> v .: "confirmed" <*> v .: "scores"
    parseJSON _ = mzero

instance ToJSON EntrantEntity where
    toJSON (IndividualEntrant eid) = object ["type" .= ("individual" :: String), "id" .= eid]
    toJSON (TeamEntrant eid) = object ["type" .= ("team" :: String), "id" .= eid]

instance FromJSON EntrantEntity where
    parseJSON (Object v) = do
        typ <- (v .: "type" :: Parser String)
        case typ of
            "individual" -> IndividualEntrant <$> v .: "id"
            "team" -> TeamEntrant <$> v .: "id"
            otherwise -> mzero
    parseJSON _ = mzero

instance ToJSON Score where
    toJSON s = object ["workoutID" .= _scoreWorkoutID s, "score" .= _scoreScore s, "type" .= _scoreType s]

instance FromJSON Score where
    parseJSON (Object v) = Score <$> v .: "workoutID" <*> v .: "score" <*> v .: "type"
    parseJSON _ = mzero

encodeRev :: Entity e => e -> [(T.Text, Value)]
encodeRev e = maybeToList $ fmap ((.=) "_rev") (view (entityRev e) e)
