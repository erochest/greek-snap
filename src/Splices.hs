{-# LANGUAGE OverloadedStrings #-}


module Splices where


import           Application
import qualified Data.Text               as T
import           Database.Persist
import           Heist
import qualified Heist.Interpreted       as I
import           Model
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Persistent
import           Utils


documentListSplices :: [Entity Document] -> Splices (SnapletISplice App)
documentListSplices docs = "documentList" ## (bindDocuments docs)

bindDocuments :: [Entity Document] -> SnapletISplice App
bindDocuments = I.mapSplices $ I.runChildrenWith . documentSplices

documentSplices :: Monad m => Entity Document -> Splices (I.Splice m)
documentSplices e@(Entity _ (Document title content _)) = do
    "documentId"      ## I.textSplice (maybe "" (T.pack . show) $ intKey e)
    "documentTitle"   ## I.textSplice title
    "documentContent" ## I.textSplice content


