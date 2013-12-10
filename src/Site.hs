{-# LANGUAGE FlexibleContexts  #-}
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
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Resource
import           Data.ByteString                             (ByteString)
import           Data.Int
import           Data.Monoid
import qualified Data.Text                                   as T
import qualified Data.Text.Encoding                          as E
import           Data.Text.Read                              (decimal)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.Sql
import           Heist
import qualified Heist.Interpreted                           as I
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Fay
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Persistent
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           System.Environment
import           Web.Heroku
------------------------------------------------------------------------------
import           Application
import           Model



------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("",                                serveDirectory "static")
         , ("/documents/",                     with pg handleDocumentList)
         , ("/documents/:documentId/",         with pg handleDocument)
         , ("/documents/:documentId/download", with pg handleDownloadDocument)
         , ("/split/",                         handleSplit)
         ]

handleDocumentList :: Handler App PersistState ()
handleDocumentList = do
    docs <- runPersist $ selectList [] [Asc DocumentTitle]
    renderWithSplices "document_list" $ documentListSplices docs

handleDocument :: Handler App PersistState ()
handleDocument = withDocument $ renderWithSplices "document" . documentSplices

handleDownloadDocument :: Handler App PersistState ()
handleDownloadDocument = withDocument $ render'
    where
        render' (Entity _ d) = do
            let filename = maybe filename E.encodeUtf8
                         . rightMay
                         . lastErr "error"
                         . T.splitOn "/"
                         $ documentSourceFile d
            modifyResponse $
                addHeader "Content-Disposition" $
                    "attachment; filename=" <> filename
            writeText $ documentContent d

handleSplit :: Handler App App ()
handleSplit = do
    docs <- with pg $ runPersist $ selectList [] [Asc DocumentTitle]
    with fay $ do
        renderWithSplices "split_form" $ documentListSplices docs

-- | Splices

documentListSplices :: [Entity Document] -> Splices (SnapletISplice App)
documentListSplices docs = "documentList" ## (bindDocuments docs)

bindDocuments :: [Entity Document] -> SnapletISplice App
bindDocuments = I.mapSplices $ I.runChildrenWith . documentSplices

documentSplices :: Monad m => Entity Document -> Splices (I.Splice m)
documentSplices e@(Entity _ (Document title content _)) = do
    "documentId"      ## I.textSplice (maybe "" (T.pack . show) $ intKey e)
    "documentTitle"   ## I.textSplice title
    "documentContent" ## I.textSplice content

-- | Utilities

withDocument :: (Entity Document -> Handler App PersistState ())
             -> Handler App PersistState ()
withDocument handler =
        getParam "documentId"
    >>= getDocument
    >>= either err handler
    where err =  const $ modifyResponse (setResponseCode 404)
              >> render "404"

getDocument :: Maybe ByteString
            -> Handler App PersistState (Either String (Entity Document))
getDocument doc = runEitherT $ do
    bsid <-  doc ?? "Missing ID."
    key  <-  Key . PersistInt64 . fst
         <$> hoistEither (decimal $ E.decodeUtf8 bsid)
    ent  <-  runPersist (get key) !? "Missing document."
    return $ Entity key ent

intKey :: Entity a -> Maybe Int64
intKey (Entity k _) = case unKey k of
                          PersistInt64 int -> Just int
                          _                -> Nothing

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "Chopped up Platonic dialogues." Nothing $
    App <$> nestSnaplet "" heist (heistInit "templates")
        <*> nestSnaplet "sess" sess
                (initCookieSessionManager "site_key.txt" "sess" (Just 3600))
        <*> nestSnaplet "" pg
                (initPersistEnv $ runMigrationUnsafe migrateAll)
        <*> nestSnaplet "fay" fay initFay

    <*  addRoutes routes

-- | This is taken from Snap.Snaplet.Persist, but I've changed it to read
-- configuration from the environment.
initPersistEnv :: SqlPersistT (NoLoggingT IO) a -> SnapletInit b PersistState
initPersistEnv migration = makeSnaplet "persist" description datadir $ do
    p <- mkEnvPool
    liftIO . runNoLoggingT $ runSqlPool migration p
    return $ PersistState p
    where description = "Snaplet for persistent DB library"
          datadir     = Nothing

mkEnvPool :: (Functor (m b v), MonadIO (m b v), MonadSnaplet m)
          => m b v ConnectionPool
mkEnvPool = do
    poolSize <-  either (const 3) id
             .   maybe (Right 3) (fmap fst . decimal . T.pack)
             <$> liftIO (lookupEnv "DATABASE_POOL_SIZE")
    dbParams <-  connect <$> liftIO dbConnParams
    createPostgresqlPool dbParams poolSize
    where connect = E.encodeUtf8 . T.intercalate " " . map eqPair
          eqPair (k, v) = k <> "=" <> v

