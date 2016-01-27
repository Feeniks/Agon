{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Agon.Types where

import Control.Lens (Lens)
import Control.Lens.TH
import Crypto.Cipher.AES (AES)
import Network.HTTP.Conduit (Manager)

class Entity e where
    entityRev :: e -> Lens e e Rev Rev
    entityID :: e -> Lens e e ID ID

type Rev = Maybe String
type ID = String

data Agon = Agon {
    _agonAES :: AES,
    _agonManager :: Manager,
    _agonCouchBaseURL :: String,
    _agonCouchDBURL :: String
}

data Principal = Principal String Int

data LoginInfo = LoginInfo User String

data AgonResult = AgonResult String

data User = User {
    _userRev :: Rev,
    _userID :: ID,
    _userName :: String,
    _userIsAdmin :: Bool
}

data Team = Team {
    _teamRev :: Rev,
    _teamID :: ID,
    _teamName :: String,
    _teamMembers :: [TeamMember],
    _teamInvitations :: [ID]
}

data TeamMember = AdminMember ID | Member ID deriving Eq

data Event = Event {
    _eventRev :: Rev,
    _eventID :: ID,
    _eventVenueName :: String,
    _eventVenueLatitude :: Double,
    _eventVenueLongitude :: Double,
    _eventVisible :: Bool,
    _eventStartTimeUTC :: Int,
    _eventEndTimeUTC :: Int,
    _eventType :: EventType,
    _eventEntryFee :: Double,
    _eventIsInvitational :: Bool,
    _eventMaxEntrants :: Int,
    _eventName :: String,
    _eventDescription :: String
}

data EventType = IndividualEvent | TeamEvent Int deriving Eq

data Workout = Workout {
    _workoutRev :: Rev,
    _workoutID :: ID,
    _workoutEventID :: ID,
    _workoutScoreType :: ScoreType,
    _workoutTimeUTC :: Int,
    _workoutName :: String,
    _workoutDescription :: String
}

data ScoreType = ScoreTime | ScoreReps | ScoreWeight deriving Eq

data Entrant = Entrant {
    _entrantRev :: Rev,
    _entrantID :: ID,
    _entrantEventID :: ID,
    _entrantEntity :: EntrantEntity,
    _entrantConfirmed :: Bool,
    _entrantScores :: [Score]
}

data EntrantEntity = IndividualEntrant ID | TeamEntrant ID deriving Eq

data Score = Score {
    _scoreWorkoutID :: ID,
    _scoreScore :: Int,
    _scoreType :: ScoreType --To allow ranking via mapreduce
}

makeLenses ''Agon
makeLenses ''User
makeLenses ''Team
makeLenses ''Event
makeLenses ''Workout
makeLenses ''Entrant
makeLenses ''Score
