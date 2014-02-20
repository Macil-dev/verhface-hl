{-#LANGUAGE DeriveDataTypeable#-}
module Bindings.Verba.HL.Types where

import Data.Typeable (Typeable)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime ()

import Foreign (Word16)

import Control.Exception (Exception)

data USR_KEYS_INFO = USR_KEYS_INFO { _num  :: String
                                   , _nump :: String
                                   , _keyStatus :: Word16
                                   , _version :: (Int, Int)
                                   , _slotNum :: Int
                                   } deriving (Eq, Show, Typeable)

data SIGN_STATUS = SIGN_STATUS_CORRECT
                 | SIGN_STATUS_NOT_CORRECT
                 | SIGN_STATUS_OKEY_NOT_FOUND
                 deriving (Eq, Show, Typeable)

data Check_Status = Check_Status { _name  :: String
                                 , _alias :: String
                                 , _position :: Int
                                 , _status :: SIGN_STATUS
                                 , _date :: Maybe UTCTime
                                 } deriving (Eq, Show, Typeable)

data S_or_E = S | E deriving (Eq, Show, Typeable)

data VerbaException = VerbaException {_verbaError :: Int} deriving (Eq, Show, Typeable)

instance Exception VerbaException

