{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Handler.Split
    ( routes
    ) where


import           Application
import           Codec.Archive.Zip
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as C8
import           Data.ByteString.Lazy      (fromStrict)
import           Data.List                 (foldl')
import           Data.Monoid
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as E
import           Data.Time.Clock.POSIX
import           Data.Traversable          (sequenceA)
import           Database.Persist
import qualified Filesystem.Path.CurrentOS as FS
import           Model
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Fay
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Persistent
import           Splices
import           XML.Split
import qualified XML.Split                 as S


-- data Hole = Hole


routes :: [(ByteString, Handler App App ())]
routes = [ ("/split/",                         handleSplit)
         , ("/split/download",                 with pg handleSplitDownload)
         , ("/fay/split.js",                   with fay fayServe)
         ]

handleSplit :: Handler App App ()
handleSplit = do
    docs <- with pg $ runPersist $ selectList [] [Asc DocumentTitle]
    with fay $ do
        renderWithSplices "split_form" $ documentListSplices docs

handleSplitDownload :: Handler App PersistState ()
handleSplitDownload = do
    docIds      <- join . fmap paramInts <$> getParam "document"
    division    <- join . fmap (S.fromText . E.decodeUtf8) <$> getParam "division"
    chunkSize   <- toint <$> getParam "chunksize"
    chunkOffset <- toint <$> getParam "chunkoffset"
    splits      <- sequenceA $ splitParams <$> docIds
                                           <*> division
                                           <*> chunkSize
                                           <*> chunkOffset
    let base = maybe "divided" (T.append "by-" . T.toLower . T.pack . show)
                     division
    maybe error500 (renderSplits base) splits
    where paramInts = mapM (fmap fst . C8.readInt) . C8.words
          toint v   = fmap fst . C8.readInt =<< v
          renderSplits base ss = do
              modifyResponse $ do
                setHeader "content-type" "application/zip"
                setHeader "content-disposition" $
                    "attachment; filename=" <> E.encodeUtf8 base <> ".zip"
              writeLBS . fromArchive =<< liftIO (splitsToArchive base ss)
          error500 = do
              modifyResponse (setResponseStatus 500 "Internal Server Error")
              writeBS "500 error"

splitParams :: [Int] -> Division -> Int -> Int -> Handler App PersistState [Split]
splitParams docIds dv size offset = do
    docs <- runPersist $ selectList [DocumentId <-. map tokey docIds] []
    liftIO . fmap concat . forM docs $ \(Entity _ Model.Document{..}) ->
        splitDocument dv (size, offset) (FS.fromText documentSourceFile)
                      documentContent
    where tokey = Key . PersistInt64 . fromIntegral

splitsToArchive :: T.Text -> [Split] -> IO Archive
splitsToArchive dir ss = do
    now <- truncate <$> getPOSIXTime
    return . foldl' (flip addEntryToArchive) emptyArchive
           $ map (splitEntry now) ss
    where splitEntry now s@Split{..} = toEntry (spath s) now
                                    . fromStrict
                                    $ E.encodeUtf8 _splitText
          dir' = FS.fromText dir
          spath Split{..} = FS.encodeString
                          $ dir' FS.</> ( FS.fromText
                                        $ T.intercalate "-"
                                        [ toText' _splitPath
                                        , _splitId
                                        , T.pack (show _splitN)
                                        ]) FS.<.> "txt"
          toText' = either id id . FS.toText

