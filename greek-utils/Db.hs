{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Db where


import           Data.Text
import           Database.Persist.Quasi
import           Database.Persist.TH


share [mkPersist sqlSettings { mpsGenerateLenses = True }, mkMigrate "migrateAll"][persistLowerCase|
TokenType
    text Text
    deriving Show
    UniqueTokenType text
TokenPosition
    tokenTypeId TokenTypeId
    filename Text
    startLine Int
    startCol Int
    endLine Int
    endCol Int
    UniquePosition filename startLine startCol
    deriving Show
|]

