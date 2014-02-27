module Bindings.Verba.HL.Helpers where

import Data.ByteString.Char8
import qualified Data.ByteString.Unsafe as BSU

import System.IO.Unsafe

import qualified GHC.IO.Encoding as GHC
import qualified GHC.Foreign as GHC

-- Every time someone write `unsafePerformIO` a cute kitty dies!

cp1251 = unsafePerformIO $ GHC.mkTextEncoding "cp1251"

decodeCp1251 :: ByteString -> String
decodeCp1251 bs =
    unsafePerformIO $ BSU.unsafeUseAsCStringLen bs $ \str ->
                          GHC.peekCStringLen cp1251 str
