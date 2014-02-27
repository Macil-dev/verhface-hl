module Bindings.Verba.HL.Ops where

import Bindings.Verba.HL.Types
import Bindings.Verba.HL.Helpers

import Bindings.Verba.C

import Control.Applicative

import Control.Exception

import Foreign
import Foreign.C

import qualified Data.ByteString.Char8 as BSC8

import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Clock.POSIX

verbaResult :: Word16 -> IO ()
verbaResult 0 = return ()
verbaResult err = throw $ VerbaException $ fromIntegral err

cryptoInit :: String -> String -> IO ()
cryptoInit openKeyPath secretKeyPath =
    withCString openKeyPath $ \openKeyPath' ->
        withCString secretKeyPath $ \secretKeyPath' ->
            verbaResult =<< c_CryptoInit openKeyPath' secretKeyPath'

signInit :: String -> String -> IO ()
signInit openKeyPath secretKeyPath =
    withCString openKeyPath $ \openKeyPath' ->
        withCString secretKeyPath $ \secretKeyPath' ->
            verbaResult =<< c_SignInit openKeyPath' secretKeyPath'

cryptoDone :: IO Word16
cryptoDone = c_CryptoDone

signDone :: IO Word16
signDone = c_SignDone

enCryptFile :: String -> String -> Word16 -> [Word16] -> String -> IO ()
enCryptFile src dst sender rcvr series =
    withCString src $ \src' ->
        withCString dst $ \dst' ->
            withArray0 0 rcvr $ \rcvr' ->
                withCString series $ \series' ->
                    verbaResult =<< c_EnCryptFile src' dst' sender rcvr' series'

deCryptFile :: String -> String -> Word16 -> IO ()
deCryptFile src dst rcvr =
    withCString src $ \src' ->
        withCString dst $ \dst' ->
            verbaResult =<< c_DeCryptFile src' dst' rcvr

signFile :: String -> String -> String -> IO String
signFile src dst [] =
    signFile src dst "\0\0\0\0\0\0\0\0\0\0\0\0"
signFile src dst key | length key == 12=
    withCString src $ \src' ->
        withCString dst $ \dst' ->
            withCString key $ \key' -> do
                res <- c_SignFile src' dst' key'
                verbaResult res
                peekCAString key'
signFile src dst key | otherwise = error "Invalid length"

check_file_sign :: String -> IO [Check_Status]
check_file_sign path =
    withCString path $ \path' ->
        alloca $ \n ->
            alloca $ \status -> do
            verbaResult =<< c_check_file_sign path' n status
            status' <- peek status
            n'      <- fromIntegral <$> peek n
            res <- peekArray n' status'
            c_Free_Memory status'
            return $ map readCheck_Status res

delSign :: String -> Word8 -> IO ()
delSign path count =
    withCString path $ \path' ->
        verbaResult =<< c_DelSign path' count

getDrvInfo :: IO [USR_KEYS_INFO]
getDrvInfo =
    allocaArray 16 $ \buff ->
        alloca $ \n -> do
            verbaResult =<< c_GetDrvInfo buff n
            n' <- fromIntegral <$> peek n
            map readUSR_KEYS_INFO <$> peekArray n' buff

writeS_or_E S = 83
writeS_or_E E = 69

readS_or_E 83 = 'S'
readS_or_E 69 = 'E'

sprList dir series soe =
    withCString dir $ \dir' ->
        withCString series $ \series' ->
            alloca $ \ptr ->
                alloca $ \n -> do
                    verbaResult =<< c_SprList dir' series' ptr n (writeS_or_E soe)
                    spr <- peek ptr
                    n'  <- fromIntegral <$> peek n
                    res <- peekArray n' spr
                    c_Free_Memory spr
                    return res

getFileSenderId :: String -> IO String
getFileSenderId path =
    withCString path $ \path' ->
        withCString "\0\0\0\0\0\0\0\0\0\0\0" $ \sender_id -> do
            verbaResult =<< c_GetFileSenderId path' sender_id 
            return =<< peekCAString sender_id

getCryptKeysF :: String -> IO ([Word16], String)
getCryptKeysF path =
    withCString path $ \path' ->
        alloca $ \n ->
            alloca $ \ptr ->
                withCString "\0\0\0\0\0\0\0" $ \series -> do
                    verbaResult =<< c_GetCryptKeysF path' n ptr series
                    user_list <- peek ptr
                    n'  <- fromIntegral <$> peek n
                    res <- peekArray n' user_list
                    series' <- peekCAString series
                    c_Free_Memory user_list
                    return (res, series')

            

readUSR_KEYS_INFO info =
    USR_KEYS_INFO (BSC8.unpack $ _c_num info)
                  (BSC8.unpack $ _c_nump info)
                  (fromIntegral $ _c_key_status info)
                  (makeVersion)
                  (fromIntegral $ _c_slot_no info)
    where
        makeVersion = ( fromIntegral . _c_ver_hi $ info
                      , fromIntegral . _c_ver_lo $ info)

readSignStatus 0 = SIGN_STATUS_CORRECT
readSignStatus 1 = SIGN_STATUS_NOT_CORRECT
readSignStatus 2 = SIGN_STATUS_OKEY_NOT_FOUND

readCheck_Status status =
    Check_Status (BSC8.unpack $ _c_name status)
                 (reverse . dropWhile (== ' ') . reverse . decodeCp1251 $ _c_alias status)
                 (fromIntegral $ _c_position status)
                 (readSignStatus $ _c_status status)
                 (case fromIntegral $ _c_date status of
                       0 -> Nothing
                       t -> Just $ posixSecondsToUTCTime t)
