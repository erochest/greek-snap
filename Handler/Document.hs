module Handler.Document where


import Import


getDocumentListR :: Handler Html
getDocumentListR = do
    docs <- runDB $ selectList [] [Asc DocumentTitle]
    defaultLayout $ do
        $(widgetFile "document_list")

getDocumentR :: DocumentId -> Handler Html
getDocumentR = undefined

getDocumentDownloadR :: DocumentId -> Handler TypedContent
getDocumentDownloadR = undefined

