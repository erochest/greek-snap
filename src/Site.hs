{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Error
import           Control.Monad
import Control.Monad.Trans.Class
import           Data.ByteString                             (ByteString)
import           Data.Int
import qualified Data.Text                                   as T
import qualified Data.Text.Encoding                          as E
import           Data.Text.Read                              (decimal)
import           Database.Persist
import           Database.Persist.Sql
import           Heist
import qualified Heist.Interpreted                           as I
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Persistent
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
------------------------------------------------------------------------------
import           Application
import           Model



------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("",                        serveDirectory "static")
         , ("/documents/",             with pg handleDocumentList)
         , ("/documents/:documentId/", with pg handleDocument)
         ]

handleDocumentList :: Handler App PersistState ()
handleDocumentList = do
    docs <- runPersist $ selectList [] [Asc DocumentTitle]
    renderWithSplices "document_list" $ documentListSplices docs

documentListSplices :: [Entity Document] -> Splices (SnapletISplice App)
documentListSplices docs = "documentList" ## (bindDocuments docs)

bindDocuments :: [Entity Document] -> SnapletISplice App
bindDocuments = I.mapSplices $ I.runChildrenWith . documentSplices

documentSplices :: Monad m => Entity Document -> Splices (I.Splice m)
documentSplices e@(Entity _ (Document title content _)) = do
    "documentId"      ## I.textSplice (maybe "" (T.pack . show) $ intKey e)
    "documentTitle"   ## I.textSplice title
    "documentContent" ## I.textSplice content

handleDocument :: Handler App PersistState ()
handleDocument = do
    doc <- getDocument =<< getParam "documentId"
    either (const $ modifyResponse (setResponseCode 404) >> render "404")
           (renderWithSplices "document" . documentSplices)
           doc

getDocument :: Maybe ByteString
            -> Handler App PersistState (Either String (Entity Document))
getDocument Nothing     = return $ Left "Missing ID."
getDocument (Just bsid) = runEitherT $ do
    key <-  Key . PersistInt64 . fst
        <$> hoistEither (decimal $ E.decodeUtf8 bsid)
    ent <- runPersist (get key) !? "Missing document."
    return $ Entity key ent

intKey :: Entity a -> Maybe Int64
intKey (Entity k _) = case unKey k of
                          PersistInt64 int -> Just int
                          _                -> Nothing



------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "Chopped up Platonic dialogues." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
            initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    p <- nestSnaplet "" pg $
            initPersist $ runMigrationUnsafe migrateAll

    addRoutes routes
    return $ App h s p

