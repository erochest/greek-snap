{-# LANGUAGE OverloadedStrings #-}


module Handler.Document
    ( routes
    ) where


import           Application
import           Control.Error
import           Data.ByteString
import           Data.Monoid
import qualified Data.Text               as T
import           Data.Text.Encoding      (encodeUtf8)
import           Database.Persist
import           Model
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Persistent
import           Splices
import           Utils


routes :: [(ByteString, Handler App App ())]
routes = [ ("/documents/",                     with pg handleDocumentList)
         , ("/documents/:documentId/",         with pg handleDocument)
         , ("/documents/:documentId/download", with pg handleDownloadDocument)
         ]

handleDocumentList :: Handler App PersistState ()
handleDocumentList = do
    docs <- runPersist $ selectList [] [Asc DocumentTitle]
    renderWithSplices "document_list" $ documentListSplices docs

handleDocument :: Handler App PersistState ()
handleDocument = withDocument $ renderWithSplices "document" . documentSplices

handleDownloadDocument :: Handler App PersistState ()
handleDownloadDocument = withDocument $ \(Entity _ d) -> do
    let filename = maybe filename encodeUtf8
                 . rightMay
                 . lastErr "error"
                 . T.splitOn "/"
                 $ documentSourceFile d
    modifyResponse $
        addHeader "Content-Disposition" $
            "attachment; filename=" <> filename
    writeText $ documentContent d

