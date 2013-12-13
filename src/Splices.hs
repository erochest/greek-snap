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
import           Text.XmlHtml            as X
import           Utils


documentListSplices :: [Entity Model.Document] -> Splices (SnapletISplice App)
documentListSplices docs = "documentList" ## (bindDocuments docs)

bindDocuments :: [Entity Model.Document] -> SnapletISplice App
bindDocuments = I.mapSplices $ I.runChildrenWith . documentSplices

documentSplices :: Monad m => Entity Model.Document -> Splices (I.Splice m)
documentSplices e@(Entity _ (Model.Document title content _)) = do
    "documentId"      ## I.textSplice (maybe "" (T.pack . show) $ intKey e)
    "documentTitle"   ## I.textSplice title
    "documentContent" ## I.textSplice content

tagSplice :: Monad m => T.Text -> [(T.Text, T.Text)] -> T.Text -> HeistT n m Template
tagSplice name attrs content = return [X.Element name attrs [X.TextNode content]]


