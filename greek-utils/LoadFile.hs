{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}


module Main where


import           Control.Applicative
import           Control.Error
import           Control.Lens
import           Control.Monad
import           Control.Monad.Logger
import           Data.Conduit
import           Data.Monoid
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Text.ICU.Normalize
import           Data.Typeable               (Typeable)
import qualified Database.Persist            as P
import           Database.Persist.Postgresql (PostgresConf)
import           Database.Persist.Quasi
import           Filesystem
import           Filesystem.Path.CurrentOS
import qualified Filesystem.Path.CurrentOS   as FS
import           Options.Applicative
import           Prelude                     hiding (FilePath)
import           Text.XML                    hiding (Document)
import qualified Text.XML                    as X
import           Text.XML.Lens               hiding (Document)
import           Yesod
import qualified Yesod.Default.Config        as C

import           Utils


-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "../config/models")

parseDocument :: X.Document -> T.Text -> FilePath -> Either T.Text Document
parseDocument X.Document{..} content src =
    note (mappend "Unable to parse document: " . either id id $ FS.toText src) $
         Document <$> fmap T.strip (documentRoot ^? el "text" ./ el "body" ./ el "head" . text)
                  <*> pure (normalize NFC content)
                  <*> hush (FS.toText src)

loadFiles :: FilePath -> PersistConfigBackend PostgresConf (ResourceT (LoggingT IO)) ()
loadFiles srcDir = do
    files <- liftIO $ listDirectory srcDir
    forM_ files $ \xmlFile -> do
        doc' <- liftIO $ parseDocument <$> X.readFile def xmlFile
                                       <*> readTextFile xmlFile
                                       <*> pure xmlFile
        case doc' of
            Right doc -> do
                log_ . mappend "Loading " . either id id $ FS.toText xmlFile
                insert_ doc
            Left msg  -> log_ msg

log_ :: MonadIO m => T.Text -> m ()
log_ = liftIO . putStrLn . T.unpack

main :: IO ()
main = do
    LF{..} <- execParser opts
    dbconf <- C.withYamlEnvironment (encodeString psqlConfig) C.Development
                P.loadConfig >>= P.applyEnv
    pool   <- P.createPoolConfig (dbconf :: PostgresConf)

    runStdoutLoggingT $ runResourceT $ flip (P.runPool dbconf) pool $ do
        when clearDb $
            deleteWhere ([] :: [Filter Document])
        loadFiles xmlDir
    where
        opts = info (helper <*> loadFileConfig)
                    (  fullDesc
                    <> progDesc "This loads some XML files into a database."
                    <> header "load-file: Loads XML files into a database."
                    )

data LoadFile = LF
              { xmlDir     :: FilePath
              , psqlConfig :: FilePath
              , clearDb    :: Bool
              } deriving (Show)

loadFileConfig :: Parser LoadFile
loadFileConfig =   LF
               <$> xmlDirOption defXmlDir
               <*> fileOption (  long "psql-config"
                              <> short 'p'
                              <> metavar "PSQL_CONFIG_FILE"
                              <> value defPsqlConfig
                              <> help (  "The YAML file containing the Postgres configuration.\
                                         \ (Default = '"
                                      <> encodeString defPsqlConfig <> "'.)"
                                      )
                              )
               <*> switch     (  long "clear-db"
                              <> short 'D'
                              <> help "Clear the database before repopulating."
                              )
    where defXmlDir     = "./gk-texts/"
          defPsqlConfig = "./dialoguer/config/postgresql.yml"


