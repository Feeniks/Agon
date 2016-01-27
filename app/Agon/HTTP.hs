{-# LANGUAGE OverloadedStrings #-}

module Agon.HTTP(
    http,
    http',
    httpJSON,
    asJSON,
    buildReq
) where

import Agon.Types
import Agon.Agon
import Agon.APIConstructors

import Control.Exception (handle)
import Control.Lens (set, view)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Network.HTTP.Client (HttpException(..))
import Network.HTTP.Conduit hiding (http)
import Network.HTTP.Types.Status (statusCode, statusMessage)
import Servant

http :: Request -> AppM BL.ByteString
http r = viewA agonManager >>= safeHTTP . httpLbs r >>= return . responseBody

http' :: Request -> AppM ()
http' r = http r >> return ()

httpJSON :: FromJSON e => Request -> AppM e
httpJSON r = http r >>= asJSON

asJSON :: FromJSON b => BL.ByteString -> AppM b
asJSON b = case (decode b) of
    Nothing -> lift . left $ err500 { errBody = BL.concat ["Could not parse: ", b] }
    Just r -> return r

buildReq :: ToJSON b => String -> B.ByteString -> Maybe b -> AppM Request
buildReq url mthd mb = do
    breq <- parseUrl url
    let req = breq { method = mthd }
    maybe (return req) (\b -> return $ req { requestBody = RequestBodyLBS (encode b) }) mb

safeHTTP :: IO a -> AppM a
safeHTTP act = lift . EitherT $ handle handleHTTP (act >>= return . Right)

handleHTTP :: HttpException -> IO (Either ServantErr a)
handleHTTP (StatusCodeException stat _ _) = return . Left $ ServantErr { errHTTPCode = code, errReasonPhrase = BL.unpack reason, errBody = reason, errHeaders = [] }
    where
    code = statusCode stat
    reason = BL.fromStrict $ statusMessage stat
handleHTTP ex = return . Left $ err500 { errBody = BL.pack (show ex) }
