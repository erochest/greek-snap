{-# LANGUAGE RecordWildCards #-}


module Handler.Document where


import Import


getDocumentListR :: Handler Html
getDocumentListR = do
    docs <- runDB $ selectList [] [Asc DocumentTitle]
    defaultLayout $ do
        $(widgetFile "document_list")

getDocumentR :: DocumentId -> Handler Html
getDocumentR documentId = do
    Document{..} <- runDB $ get404 documentId
    defaultLayout $ do
        $(widgetFile "document")

getDocumentDownloadR :: DocumentId -> Handler TypedContent
getDocumentDownloadR = undefined

