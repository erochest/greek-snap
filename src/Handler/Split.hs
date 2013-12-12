{-# LANGUAGE OverloadedStrings #-}


module Handler.Split
    ( routes
    ) where


import           Application
import           Data.ByteString         (ByteString)
import           Database.Persist
import           Model
import           Snap.Snaplet
import           Snap.Snaplet.Fay
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Persistent
import           Splices


routes :: [(ByteString, Handler App App ())]
routes = [ ("/split/",                         handleSplit)
         , ("/split/download",                 with pg handleSplitDownload)
         , ("/fay/split.js",                   with fay fayServe)
         ]

handleSplit :: Handler App App ()
handleSplit = do
    docs <- with pg $ runPersist $ selectList [] [Asc DocumentTitle]
    with fay $ do
        renderWithSplices "split_form" $ documentListSplices docs

handleSplitDownload :: Handler App PersistState ()
handleSplitDownload = undefined
