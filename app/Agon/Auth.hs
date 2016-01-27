
module Agon.Auth(
    withAuth,
    withAuthOpt,
    withAuthAdmin,
    encrypt
) where

import Agon.Types
import Agon.Instances
import Agon.Agon
import Agon.APIConstructors
import Agon.Data

import Control.Lens ((^.))
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Crypto.Cipher.AES
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Base16.Lazy as B16
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word8
import Servant

withAuth :: AuthToken -> (User -> AppM a) -> AppM a
withAuth Nothing _ = lift $ left err403
withAuth (Just tok) f = do
    mp <- decrypt tok
    withPrincipalOpt mp $ \musr -> do
        case musr of
            Nothing -> lift $ left err403
            Just usr -> f usr

withAuthOpt :: AuthToken -> (Maybe User -> AppM a) -> AppM a
withAuthOpt Nothing f = f Nothing
withAuthOpt (Just tok) f = decrypt tok >>= flip withPrincipalOpt f

withAuthAdmin :: AuthToken -> (User -> AppM a) -> AppM a
withAuthAdmin tok f = withAuth tok $ \usr -> case (usr ^. userIsAdmin) of
    True -> f usr
    False -> lift $ left err403

withPrincipalOpt :: (Maybe Principal) -> (Maybe User -> AppM a) -> AppM a
withPrincipalOpt Nothing f = f Nothing
withPrincipalOpt (Just (Principal uid exp)) f = do
    tme <- liftIO $ fmap round getPOSIXTime
    case (exp > tme) of
        True -> get uid >>= f . Just
        False -> f Nothing

encrypt :: Monad m => Principal -> AgonM m String
encrypt p = do
    aes <- viewA agonAES
    return . B.unpack . B16.encode $ enc aes (encode p)

decrypt :: Monad m => String -> AgonM m (Maybe Principal)
decrypt t = do
    aes <- viewA agonAES
    let bs = fst . B16.decode $ B.pack t
    case B.length bs of
        0 -> return Nothing
        _ -> maybe (return Nothing) (return . decode) (dec aes bs)

enc :: AES -> B.ByteString -> B.ByteString
enc aes input = (B.fromStrict . encryptECB aes . B.toStrict) (BS.append (pad input) input)
    where
    drem = fromIntegral $ 16 - (mod (B.length input) 16)
    pad ix
        | drem == 16 = BS.pack $ [16] ++ replicate 15 0
        | otherwise = BS.pack $ [fromIntegral $ drem] ++ replicate (drem - 1) 0

dec :: AES -> B.ByteString -> Maybe B.ByteString
dec aes input
    | mod (B.length input) 16 /= 0 = Nothing
    | otherwise = Just $ unpad (B.fromStrict $ decryptECB aes $ B.toStrict input)
    where
    unpad ct = BS.drop (fromIntegral $ BS.head ct) ct
