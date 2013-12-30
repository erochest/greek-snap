{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Handler.Document where


import           Blaze.ByteString.Builder.ByteString
import           Control.Error
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Import


getDocumentListR :: Handler Html
getDocumentListR = do
    docs <- runDB $ selectList [] [Asc DocumentTitle]
    defaultLayout $ do
        $(widgetFile "document_list")

getDocumentR :: DocumentId -> Handler Html
getDocumentR documentId = do
    Document{..} <- runDB $ get404 documentId
    defaultLayout $ do
        setTitle $ toHtml documentTitle
        $(widgetFile "document")

getDocumentDownloadR :: DocumentId -> Handler RepXml
getDocumentDownloadR documentId = do
    Document{..} <- runDB $ get404 documentId
    let filename = fromMaybe filename
                 . rightMay
                 . lastErr ("error" :: T.Text)
                 . T.splitOn "/"
                 $ documentSourceFile
    addHeader "Content-Disposition" $ "attachment; filename=" <> filename
    return . RepXml . flip ContentBuilder Nothing . fromByteString
           $ encodeUtf8 documentContent
