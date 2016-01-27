{-# LANGUAGE OverloadedStrings #-}

import Agon.Server

import Agon.Types

import Crypto.Cipher.AES
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Base16 as B16
import Network.HTTP.Conduit
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Static
import System.Environment

main :: IO ()
main = do
    port <- fmap read $ getEnv "PORT"
    couchURL <- getEnv "AGON_COUCH_URL"
    couchDB <- getEnv "AGON_DB"
    aesKey <- fmap B.pack $ getEnv "AGON_AES_KEY"

    mgr <- newManager tlsManagerSettings

    let agon = Agon {
        _agonAES = initAES . fst $ B16.decode aesKey,
        _agonManager = mgr,
        _agonCouchBaseURL = couchURL,
        _agonCouchDBURL = couchURL ++ "/" ++ couchDB ++ "/"
    }

    let stat = staticPolicy (noDots >-> (policy defIndex) >-> addBase "static")

    run port $ stat (app agon)

defIndex :: String -> Maybe String
defIndex s = case (s == "" || s == "/") of
    True -> Just "index.html"
    False -> Just s
