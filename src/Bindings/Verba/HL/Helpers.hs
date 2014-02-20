module Bindings.Verba.HL.Helpers where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

import Codec.Text.IConv

iconvConvert  =
    T.unpack . E.decodeUtf8 . BL.toStrict . (convert
                                             "cp1251"
                                             "utf-8"
                                            ) . BL.fromStrict
