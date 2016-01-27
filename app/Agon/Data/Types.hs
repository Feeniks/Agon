
module Agon.Data.Types where

data UUIDs = UUIDs [String]

data CouchUpdateResult = CouchUpdateResult {
    curRev :: String
} deriving Show

data CouchList e = CouchList {
    clItems :: [CouchListItem e]
} deriving Show

data CouchListItem e = CouchListItem {
    cliItem :: e
} deriving Show
