{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Agon.APIConstructors where

import Agon.Types
import Agon.Agon

import Control.Monad.Trans.Either
import Servant

type ServantM = EitherT ServantErr IO
type AppM = AgonM ServantM
type AuthToken = Maybe String

type AAuth = Header "A-Auth" String

type ReadAPI e = ListAPI e :<|> GetAPI e

type ReadDependentAPI e = ListDependentAPI e :<|> GetAPI e

type CreateUpdateAPI e = CreateAPI e :<|> UpdateAPI e

type ListAPI e = AAuth :> Get '[JSON] [e]

type ListDependentAPI e = AAuth :> "childrenof" :> Capture "id" ID :> Get '[JSON] [e]

type GetAPI e = AAuth :> Capture "id" String :> Get '[JSON] e

type CreateAPI e = AAuth :> ReqBody '[JSON] e :> Put '[JSON] e

type UpdateAPI e = AAuth :> ReqBody '[JSON] e :> Post '[JSON] e

type DeleteAPI = AAuth :> Capture "id" ID :> Capture "rev" ID :> Delete '[JSON] ()
