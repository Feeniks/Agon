{-# LANGUAGE OverloadedStrings #-}

module Agon.FBAuth(login) where

import Agon.Types
import Agon.Instances
import Agon.Agon
import Agon.APIConstructors
import Agon.Data
import Agon.HTTP

import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Time.Clock.POSIX (getPOSIXTime)
import Servant

data FacebookInfo = FacebookInfo {
    fbID :: String,
    fbName :: String
}

instance FromJSON FacebookInfo where
    parseJSON (Object v) = FacebookInfo <$> v .: "id" <*> v .: "name"
    parseJSON _ = mzero

login :: String -> AppM (User, Principal)
login token = do
    let url = "https://graph.facebook.com/me?access_token=" ++ token
    req <- buildReq url "GET" (Nothing :: Maybe ())
    fb <- httpJSON req
    agon <- ask
    fetchRes <- liftIO (runEitherT $ runAgonM (get $ fbID fb) agon)
    case fetchRes of
        Left err -> case (errHTTPCode err) of
            404 -> createUser fb
            _ -> lift $ left err
        Right u -> do
            p <- makePrincipal u
            return (u, p)

createUser :: FacebookInfo -> AppM (User, Principal)
createUser (FacebookInfo fid fname) = do
    let usr = User Nothing fid fname False
    update usr
    p <- makePrincipal usr
    return (usr, p)

makePrincipal :: User -> AppM Principal
makePrincipal u = do
    let uid = _userID u
    tme <- liftIO $ fmap round getPOSIXTime
    return $ Principal uid (tme + 604800)
