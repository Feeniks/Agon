{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Agon.API where

import Agon.Types
import Agon.Instances
import Agon.APIConstructors

import Data.Aeson
import qualified Data.Text as T
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Server.Internal

type AgonAPI = AuthAPI :<|> UsersAPI :<|> TeamsAPI :<|> EventsAPI :<|> WorkoutsAPI :<|> EntrantsAPI

type AuthAPI = "auth" :> Capture "token" String :> Get '[JSON] LoginInfo

type UsersAPI = "users" :> ReadAPI User

type TeamsAPI = "teams" :>
    (
        ReadAPI Team
        :<|>
        AAuth :> Capture "name" String :> Put '[JSON] Team
        :<|>
        AAuth :> Capture "id" ID :> Capture "uid" ID :> Post '[JSON] Team
        :<|>
        AAuth :> Capture "id" ID :> Post '[JSON] Team
        :<|>
        AAuth :> Capture "id" ID :> Capture "uid" ID :> Delete '[JSON] Team
    )

type EventsAPI = "events" :> (ReadAPI Event :<|> CreateUpdateAPI Event :<|> DeleteAPI)

type WorkoutsAPI = "workouts" :> (ReadDependentAPI Workout :<|> CreateUpdateAPI Workout :<|> DeleteAPI)

type EntrantsAPI = "entrants" :> (ReadDependentAPI Entrant :<|> CreateAPI Entrant :<|> (AAuth :> Capture "id" ID :> ReqBody '[JSON] Score :> Put '[JSON] Entrant))
