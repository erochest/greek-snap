{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

module Model where

import Control.Applicative ((<$>))
import Prelude
import Yesod
import Data.Text (Text)
import Database.Persist.Sql
import Database.Persist.Quasi
import Database.Persist.TH
import Data.Typeable (Typeable)

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

-- I'm certain that there are prettier type signatures available for these,
-- but I more or less copied these from GHC's output, which is never
-- pretty.

{-
 - getUserProfile :: forall (m :: * -> *).
 -                (  Functor m, PersistUnique m, PersistMonadBackend m ~ SqlBackend)
 -                => Entity User -> m (Entity UserProfile)
 -}
 getUserProfile user = do
    mprofile <- getUserProfile' user
    case mprofile of
        Just profile -> return profile
        Nothing -> let profile = UserProfile (entityKey user) False
                  in  (flip Entity profile) <$> insert profile


{-
 - getUserProfile' :: forall (m :: * -> *).
 -                 (  PersistUnique m, PersistMonadBackend m ~ SqlBackend)
 -                 => Entity User -> m (Maybe (Entity UserProfile))
 -}
getUserProfile' = getBy . UniqueUserProfile . entityKey

{-
 - isAdmin :: forall site.
 -         (  Functor (YesodPersistBackend site (HandlerT site IO))
 -         ,  PersistUnique (YesodPersistBackend site (HandlerT site IO))
 -         ,  YesodPersist site
 -         ,  PersistMonadBackend (YesodPersistBackend site (HandlerT site IO)) ~ SqlBackend)
 -         => Entity User -> HandlerT site IO Bool
 -}
isAdmin = runDB . isAdmin'

{-
 - isAdmin' :: forall (f :: * -> *).
 -          (  Functor f, PersistUnique f, PersistMonadBackend f ~ SqlBackend)
 -          => Entity User -> f Bool
 -}
isAdmin' user = userProfileIsAdmin . entityVal <$> getUserProfile user
