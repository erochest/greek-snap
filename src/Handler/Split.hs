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
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as C8
import           Data.ByteString.Lazy       (fromStrict)
import qualified Data.Char                  as C
import           Data.List                  (foldl')
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as E
import qualified Data.Text.Lazy             as TL
import           Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.Builder     as B
import           Data.Text.Lazy.Builder.Int
import           Data.Time.Clock.POSIX
import           Data.Traversable           (sequenceA)
import           Database.Persist
import qualified Filesystem.Path.CurrentOS  as FS
import           Heist
import qualified Heist.Interpreted          as I
import           Model
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Fay
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Persistent
import           Splices
import           Utils
import           XML.Split
import qualified XML.Split                  as S


-- data Hole = Hole


routes :: [(ByteString, Handler App App ())]
routes = [ ("/split/",                         handleSplit)
         , ("/split/done",                     with pg handleSplitDone)
         , ("/split/download",                 with pg handleSplitDownload)
         , ("/fay/split.js",                   with fay fayServe)
         ]

handleSplit :: Handler App App ()
handleSplit = do
    docs <- with pg $ runPersist $ selectList [] [Asc DocumentTitle]
    with fay $ do
        renderWithSplices "split_form" $ documentListSplices docs

handleSplitDone :: Handler App PersistState ()
handleSplitDone = getSplitParams >>= maybe error500 done
    where done = renderWithSplices "split_done" . spSplices . mkDlUrl
          spSplices url = do
              "downloadURL"    ## I.textSplice url
              "downloadScript" ## tagSplice "script" [("language", "javascript")]
                    ("setTimeout(function() { window.location = '" <> url <> "'; }, 3000);\n")
          mkDlUrl SP{..} =
                 TL.toStrict
              .  toLazyText
              $  mconcat [ "/split/download"
                         , "?division=",    B.fromString . map C.toLower $ show division
                         , "&chunksize=",   decimal chunkSize
                         , "&chunkoffset=", decimal chunkOffset
                         ]
              <> (mconcat . map (mappend "&document=" . decimal)
                          $ mapMaybe (intKey . flip Entity undefined) docIds)

handleSplitDownload :: Handler App PersistState ()
handleSplitDownload = do
    sp     <- getSplitParams
    splits <- sequenceA $ splitParams <$> sp
    let base = maybe "divided" (T.append "by-" . T.toLower . T.pack . show . division) sp
    maybe error500 (renderSplits base) splits
    where toint v   = fmap fst . C8.readInt =<< v
          renderSplits base ss = do
              modifyResponse $ do
                setHeader "content-type" "application/zip"
                setHeader "content-disposition" $
                    "attachment; filename=" <> E.encodeUtf8 base <> ".zip"
              writeLBS . fromArchive =<< liftIO (splitsToArchive base ss)

data SplitParams = SP
                 { docIds      :: [Key Model.Document]
                 , division    :: Division
                 , chunkSize   :: Int
                 , chunkOffset :: Int
                 } deriving (Show)

getSplitParams :: MonadSnap m => m (Maybe SplitParams)
getSplitParams = do
    docIds      <- join . fmap paramInts <$> getParam "document"
    division    <- join . fmap (S.fromText . E.decodeUtf8) <$> getParam "division"
    chunkSize   <- toint <$> getParam "chunksize"
    chunkOffset <- toint <$> getParam "chunkoffset"
    return $ SP <$> docIds <*> division <*> chunkSize <*> chunkOffset
    where tokey     = Key . PersistInt64 . fromIntegral
          paramInts = mapM (fmap (tokey . fst) . C8.readInt) . C8.words
          toint v   = fmap fst . C8.readInt =<< v

splitParams :: SplitParams -> Handler App PersistState [Split]
splitParams SP{..} = do
    docs <- runPersist $ selectList [DocumentId <-. docIds] []
    liftIO . fmap concat . forM docs $ \(Entity _ Model.Document{..}) ->
        splitDocument division
                      (chunkSize, chunkOffset)
                      (FS.fromText documentSourceFile)
                      documentContent

splitsToArchive :: T.Text -> [Split] -> IO Archive
splitsToArchive dir ss = do
    now <- truncate <$> getPOSIXTime
    return . foldl' (flip addEntryToArchive) emptyArchive
           $ map (splitEntry now) ss
    where splitEntry now s@Split{..} = toEntry (makeArchivePath dir' s) now
                                    . fromStrict
                                    $ E.encodeUtf8 _splitText
          dir' = FS.fromText dir

makeArchivePath :: FS.FilePath -> Split -> FilePath
makeArchivePath dir Split{..} =
    FS.encodeString $ dir FS.</> ( FS.fromText $ T.intercalate "-" parts)
                          FS.<.> "txt"
    where parts = [ toText' _splitPath
                  , clean _splitId
                  , T.pack (show _splitN)
                  ]
          toText' = either id id . FS.toText

clean :: T.Text -> T.Text
clean = T.filter (\c -> C.isAscii c && C.isAlphaNum c) . T.map char

char :: Char -> Char
char '\x03b1' = 'a'
char '\x03b2' = 'b'
char '\x03b3' = 'g'
char '\x03b4' = 'd'
char '\x03b5' = 'e'
char '\x03b6' = 'z'
char '\x03b7' = 'h'
char '\x03b8' = 'q'
char '\x03b9' = 'i'
char '\x03ba' = 'k'
char '\x03bb' = 'l'
char '\x03bc' = 'm'
char '\x03bd' = 'n'
char '\x03be' = 'c'
char '\x03bf' = 'o'
char '\x03c0' = 'p'
char '\x03c1' = 'r'
char '\x03c2' = 's'
char '\x03c3' = 's'
char '\x03c4' = 't'
char '\x03c5' = 'u'
char '\x03c6' = 'f'
char '\x03c7' = 'x'
char '\x03c8' = 'y'
char '\x03c9' = 'w'
char '\x03dd' = 'v'
char '\x03f2' = 's'
char c        = c

