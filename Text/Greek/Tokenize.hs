{-# LANGUAGE OverloadedStrings #-}


module Text.Greek.Tokenize
    ( grc
    , tokenize
    ) where


import qualified Data.Text     as T
import           Data.Text.ICU
import           Import


grc :: LocaleName
grc = Locale "grc"

tokenize :: T.Text -> [T.Text]
tokenize = map brkBreak
         . filter ((/= Uncategorized) . brkStatus)
         . breaks (breakWord grc)

