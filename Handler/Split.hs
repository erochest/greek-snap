{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}


module Handler.Split where


import           Codec.Archive.Zip
import           Control.Monad
import           Data.ByteString.Lazy      (fromStrict)
import qualified Data.Char                 as C
import           Data.List                 (foldl')
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as E
import           Data.Time.Clock.POSIX
import           Database.Persist
import           Import
import           Language.Haskell.TH       (Exp(..))
import qualified Filesystem.Path.CurrentOS as FS

import           Text.XML.Split
import qualified Text.XML.Split            as S


data Hole = Hole

getSplitR :: Handler Html
getSplitR = do
    docs <- documentOptions
    let formWidget = splitFormWidget docs
    defaultLayout $ do
        $(fayFile' (ConE 'StaticR) "Split")
        $(widgetFile "split")

postSplitR :: Handler Html
postSplitR = do
    urlParams <- splitUrlParams <$> runInputPost splitForm
    defaultLayout $ do
        $(widgetFile "split_done")

getSplitDownloadR :: Handler TypedContent
getSplitDownloadR = do
    sp     <- runInputGet splitForm
    splits <- liftIO $ split sp
    let base = T.append "by-" . T.toLower . T.pack . show $ splitDivision sp
    addHeader "content-disposition" $ "attachment; filename=" <> base <> ".zip"
    liftIO (archiveSplits base splits) >>= return . TypedContent "application/zip" . toContent . fromArchive

data SplitInfo = SI
               { splitDocuments   :: [Entity Document]
               , splitDivision    :: Division
               , splitChunkSize   :: Int
               , splitChunkOffset :: Int
               }

split :: SplitInfo -> IO [Split]
split SI{..} = concat <$> mapM (split' . entityVal) splitDocuments
    where split' :: Import.Document -> IO [Split]
          split' Import.Document{..} =
            S.splitDocument splitDivision
                            (splitChunkSize, splitChunkOffset)
                            (FS.fromText documentSourceFile)
                            documentContent

archiveSplits :: T.Text -> [Split] -> IO Archive
archiveSplits dir ss = do
    now <- truncate <$> getPOSIXTime
    return . foldl' (flip addEntryToArchive) emptyArchive
           $ map (splitEntry now) ss
    where splitEntry now s@Split{..} = toEntry (makeArchivePath dir' s) now
                                    . fromStrict
                                    $ E.encodeUtf8 _splitText
          dir' = FS.fromText dir

makeArchivePath :: FS.FilePath -> Split -> FilePath
makeArchivePath dir Split{..} =
    FS.encodeString $ dir FS.</> (FS.fromText $ T.intercalate "-" parts)
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


splitUrlParams :: SplitInfo -> T.Text
splitUrlParams SI{..} =
    mconcat [ "chunksize=",   showt splitChunkSize
            , "&chunkoffset=", showt splitChunkOffset
            , maybe "" (mappend "&division=" . showt)
            $ findi ((== splitDivision) . snd) 1 divisionOptions
            , docParams
            ]
    where docParams   = foldr docp mempty $ mapMaybe intKey splitDocuments
          docp p rest = "&document=" <> showt p <> rest
          showt       = T.pack . show
          intKey (Entity k _) = case unKey k of
                                    PersistInt64 int -> Just int
                                    _                -> Nothing
          findi :: (a -> Bool) -> Int -> [a] -> Maybe Int
          findi _ _ []                 = Nothing
          findi p i (x:xs) | p x       = Just i
                           | otherwise = findi p (i + 1) xs

splitFormWidget :: OptionList (Entity Document) -> Widget
splitFormWidget docs = do
    $(whamletFile "templates/split_form.hamlet")

documentOptions :: Handler (OptionList (Entity Document))
documentOptions = optionsPersist [] [Asc DocumentTitle] documentTitle

divisionOptions :: [(T.Text, Division)]
divisionOptions = [ ("Document", S.Document)
                  , ("Section",  Section)
                  , ("Page",     Page)
                  , ("Speaking", Speaking)
                  ]

splitForm :: FormInput Handler SplitInfo
splitForm = SI <$> ireq (multiSelectField documentOptions) "document"
               <*> ireq (radioFieldList divisionOptions)   "division"
               <*> ireq intField                           "chunksize"
               <*> ireq intField                           "chunkoffset"

