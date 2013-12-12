{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.ByteString                             (ByteString)
import           Data.Monoid
import qualified Data.Text                                   as T
import qualified Data.Text.Encoding                          as E
import           Data.Text.Read                              (decimal)
import           Database.Persist.Postgresql
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
import qualified Handler.Document
import qualified Handler.Split
import           Model                                       hiding (Document)


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("",                                serveDirectory "static")
         ]
         ++ Handler.Document.routes
         ++ Handler.Split.routes

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
    liftIO . void . runNoLoggingT $ runSqlPool migration p
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

