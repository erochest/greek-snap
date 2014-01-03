{-# LANGUAGE OverloadedStrings #-}


module Handler.Version where


import           Data.Version
import           Import

import           Paths_greek_snap (version)


getVersionR :: Handler TypedContent
getVersionR = selectRep $ do
    provideRep $ defaultLayout $ do
        $(widgetFile "version")


