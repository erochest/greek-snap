{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
module SharedTypes where

import Prelude
import Data.Data
#ifdef FAY
import FFI
#else
import Fay.FFI
#endif
import Language.Fay.Yesod

data Command = Noop (Returns ())
    deriving (Read, Typeable, Data)

