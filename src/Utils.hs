{-# LANGUAGE OverloadedStrings #-}


module Utils where


import           Application
import           Control.Applicative
import           Control.Error
import           Data.ByteString
import           Data.Int
import           Data.Text.Encoding      (decodeUtf8)
import           Data.Text.Read          (decimal)
import           Database.Persist
import           Model
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Persistent


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
         <$> hoistEither (decimal $ decodeUtf8 bsid)
    ent  <-  runPersist (get key) !? "Missing document."
    return $ Entity key ent

intKey :: Entity a -> Maybe Int64
intKey (Entity k _) = case unKey k of
                          PersistInt64 int -> Just int
                          _                -> Nothing

