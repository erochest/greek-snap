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
import           Data.ByteString                             (ByteString)
import qualified Data.Text                                   as T
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
         , ("/documents/:documentId/", handleDocument)
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
documentSplices (Entity dId (Document title _ _)) = do
    "documentId"    ## I.textSplice (T.pack $ show dId)
    "documentTitle" ## I.textSplice title

handleDocument :: Handler App App ()
handleDocument = undefined


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

