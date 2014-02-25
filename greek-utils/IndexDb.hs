{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}


module Main where


import qualified Data.List as L
import           Control.Monad
import           Control.Monad.Logger
import           Data.Conduit
import qualified Data.HashMap.Strict         as M
import           Data.Maybe
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.IO                as TIO
import           Data.Typeable               (Typeable)
import           Database.Persist
import qualified Database.Persist            as P
import           Database.Persist.Postgresql (PostgresConf)
import           Database.Persist.Quasi
import           Filesystem.Path.CurrentOS
import qualified Filesystem.Path.CurrentOS   as FS
import           Options.Applicative
import qualified Options.Applicative         as O
import           Prelude                     hiding (FilePath)
import           Yesod
import qualified Yesod.Default.Config        as C

import           IndexXml.Context
import           IndexXml.Index
import           IndexXml.Output
import           IndexXml.Types
import           Utils


share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "../config/models")



main :: IO ()
main = do
    ID{..} <- execParser indexDbOpts

    dbconf <- C.withYamlEnvironment (encodeString psqlConfig) C.Development
                P.loadConfig >>= P.applyEnv
    pool   <- P.createPoolConfig (dbconf :: PostgresConf)
    docs   <- fmap (map entityVal) . runStdoutLoggingT $
        runResourceT $ flip (P.runPool dbconf) pool $
            selectList [] [Asc DocumentTitle]

    let dindex = L.foldl' insertFile M.empty docs
        getter = return
               . fromMaybe ""
               . flip M.lookup dindex
               . either id id
               . FS.toText

    mapM_ TIO.putStr
        .   formatContext
        .   combineChildren
        .   fromMaybe (QC queryText [])
        =<< makeContext getter queryText contextN
        =<< foldM indexDocument M.empty
        (   map doctuple docs)

    where doctuple Document{..} = (documentSourceFile, documentContent)
          insertFile i Document{..} =
              M.insert documentSourceFile documentContent i


data IndexDb = ID
             { psqlConfig :: FilePath
             , contextN   :: Int
             , queryText  :: T.Text
             } deriving (Show)

indexDbConfig :: Parser IndexDb
indexDbConfig =   ID
              <$> fileOption (  long "psql-config"
                             <> short 'p'
                             <> metavar "PSQL_CONFIG_FILE"
                             <> value defPsqlConfig
                             <> help (  "The YAML file containing the Postgres configuration.\
                                        \ (Default = '"
                                     <> encodeString defPsqlConfig <> "'.)"))
              <*> option (  short 'c'
                         <> long "context"
                         <> metavar "CONTEXT_LINES"
                         <> value 2
                         <> help "The number of context lines before and after\
                                 \ each hit. (default = 2)")
             <*> O.argument (Just . T.pack)
                            (metavar "QUERY" <> help "The query to search for.")
    where defPsqlConfig = "../config/postgresql.yml"

indexDbOpts :: ParserInfo IndexDb
indexDbOpts = info (helper <*> indexDbConfig)
                   (  fullDesc
                   <> progDesc "This indexes and searches the XML in the database."
                   <> header "index-db: Search XML in the database.")
