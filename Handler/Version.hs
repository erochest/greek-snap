{-# LANGUAGE OverloadedStrings #-}


module Handler.Version where


import           Control.Error
import qualified Data.Text as T
import           Data.Version
import           Import

import           Paths_greek_snap (version)


getVersionLevel :: Int -> Version -> Int
getVersionLevel n = either id id . (flip (atErr 0) n) . versionBranch

getVersionTags :: Version -> Maybe T.Text
getVersionTags = go . versionTags
    where go [] = Nothing
          go xs = Just . T.intercalate "." $ map T.pack xs

getVersionR :: Handler TypedContent
getVersionR = selectRep $ do
    provideRep $ defaultLayout $ do
        $(widgetFile "version")
    provideRep $ return $ object
        [ "major" .= getVersionLevel 0 version
        , "minor" .= getVersionLevel 1 version
        , "patch" .= getVersionLevel 2 version
        -- , "build" .= getVersionLevel 3 version
        , "tags"  .= getVersionTags version
        ]


