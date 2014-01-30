{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Import
import Settings
-- import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Network.Wai.Middleware.RequestLogger
    ( mkRequestLogger, outputFormat, OutputFormat (..), IPAddrSource (..), destination
    )
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import qualified Database.Persist
import Database.Persist.Sql (runMigration)
import Network.HTTP.Conduit (newManager, conduitManagerSettings)
import Control.Monad.Logger (runLoggingT)
import System.Log.FastLogger (newStdoutLoggerSet, defaultBufSize)
import Network.Wai.Logger (clockDateCacher)
import Data.Default (def)
import Yesod.Core.Types (loggerSet, Logger (Logger))
import Data.HashMap.Strict as H
import Data.Aeson.Types as AT
#ifndef DEVELOPMENT
import qualified Web.Heroku
#endif

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Document
import Handler.Home
import Handler.Split
import Handler.StopList
import Handler.Version

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> IO Application
makeApplication conf = do
    foundation <- makeFoundation conf

    -- Initialize the logging middleware
    logWare <- mkRequestLogger def
        { outputFormat =
            if development
                then Detailed True
                else Apache FromSocket
        , destination = RequestLogger.Logger $ loggerSet $ appLogger foundation
        }

    -- Create the WAI application and apply middlewares
    app <- toWaiAppPlain foundation
    return $ logWare app

-- | Loads up any necessary settings, creates your foundation datatype, and
-- performs some initialization.
makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    manager <- newManager conduitManagerSettings
    s <- staticSite
    hconfig <- loadHerokuConfig
    dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
              (Database.Persist.loadConfig . combineMappings hconfig) >>=
              Database.Persist.applyEnv
    p <- Database.Persist.createPoolConfig (dbconf :: Settings.PersistConf)

    loggerSet' <- newStdoutLoggerSet defaultBufSize
    (getter, _) <- clockDateCacher

    let logger = Yesod.Core.Types.Logger loggerSet' getter
        foundation = App conf s p manager dbconf logger

    -- Perform database migration using our application's logging settings.
    runLoggingT
        (Database.Persist.runPool dbconf (runMigration migrateAll) p)
        (messageLoggerSource foundation logger)

    return foundation

#ifndef DEVELOPMENT
canonicalizeKey :: (Text, val) -> (Text, val)
canonicalizeKey ("dbname", val) = ("database", val)
canonicalizeKey pair            = pair

toMapping :: [(Text, Text)] -> AT.Value
toMapping xs = AT.Object $ H.fromList $ Import.map (\(key, val) -> (key, AT.String val)) xs
#endif

combineMappings :: AT.Value -> AT.Value -> AT.Value
combineMappings (AT.Object m1) (AT.Object m2) = AT.Object $ m1 `H.union` m2
combineMappings _ _ = error "Data.Object is not a Mapping."

loadHerokuConfig :: IO AT.Value
loadHerokuConfig = do
#ifdef DEVELOPMENT
    return $ AT.Object H.empty
#else
    Web.Heroku.dbConnParams >>= return . toMapping . Import.map canonicalizeKey
#endif


-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader makeApplication
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
