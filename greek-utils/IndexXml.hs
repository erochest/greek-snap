{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS_GHC -Wall #-}

-- TODOS:
-- [ ] combine hit contexts
-- [ ] highlighting
-- [ ] highlighting options


module Main where


import           Control.Applicative
import           Control.Arrow
import           Control.Lens
import           Control.Monad
import qualified Data.Char                 as C
import           Data.Conduit
import           Data.Conduit.Attoparsec   (Position (..), PositionRange (..))
import qualified Data.Conduit.Binary       as CB
import qualified Data.Conduit.List         as CL
import qualified Data.DList                as D
import qualified Data.HashMap.Strict       as M
import qualified Data.List                 as L
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                 as T
import           Data.Text.ICU
import qualified Data.Text.IO              as TIO
import           Data.XML.Types
import           Filesystem
import           Filesystem.Path.CurrentOS hiding (concat, filename)
import qualified Filesystem.Path.CurrentOS as FS
import           Options.Applicative       hiding ((&))
import qualified Options.Applicative       as O
import           Prelude                   hiding (FilePath, break, lines)
import           Text.XML.Stream.Parse

import           Utils

import           Debug.Trace


type FileLocation  = (FilePath, PositionRange)
type InvertedIndex = M.HashMap T.Text (D.DList FileLocation)

data Hit
data File
data Query

data ResultContext a where
    HC :: { _contextLocation :: PositionRange
          , _contextCount    :: Int
          , _contextRange    :: (Int, Int)
          , _contextLines    :: [T.Text]
          }                                -> ResultContext Hit
    FC :: { _contextFile :: FilePath
          , _fileHits    :: [ResultContext Hit]
          }                                -> ResultContext File
    QC :: { _queryTerm  :: T.Text
          , _queryFiles :: [ResultContext File]
          }                                -> ResultContext Query
makeLenses ''ResultContext

data BuilderInfo a where
    HB :: { hitContext  :: Int
          , hitLines    :: [T.Text]
          , hitPRange   :: PositionRange
          }                             -> BuilderInfo Hit
    FB :: { fileContext :: Int
          , fileLocs    :: [FileLocation]
          }                             -> BuilderInfo File
    QB :: { qText       :: T.Text
          , qContext    :: Int
          , qIndex      :: InvertedIndex
          }                             -> BuilderInfo Query

grc :: LocaleName
grc = Locale "grc"

indexFile :: InvertedIndex -> FilePath -> IO InvertedIndex
indexFile iindex xmlFile = runResourceT $
       CB.sourceFile (FS.encodeString xmlFile)
    $= parseBytesPos def
    $= CL.filter (isContentEvent . snd)
    $= CL.filter (isJust . fst)
    $= CL.map (fmap getText)
    $= CL.concatMap (spread tokenizeText)
    $= CL.map offsetWord
    $$ CL.fold (indexTokens xmlFile) iindex

isContentEvent :: Event -> Bool
isContentEvent (EventContent _) = True
isContentEvent (EventCDATA _)   = True
isContentEvent _                = False

getText :: Event -> T.Text
getText (EventContent (ContentText t))   = t
getText (EventContent (ContentEntity t)) = "&" <> t <> ";"
getText (EventCDATA t)                   = t
getText _                                = mempty

tokenizeText :: T.Text -> [Break Word]
tokenizeText = filter (not . T.all C.isSpace . brkBreak)
             . filter ((/= Uncategorized) . brkStatus)
             . breaks (breakWord grc)

spread :: (b -> [c]) -> (a, b) -> [(a, c)]
spread f (a, b) = map (a,) $ f b

offset :: Break Word -> PositionRange -> PositionRange
offset break (PositionRange start _) = PositionRange start' end'
    where posPlus (Position l c) = uncurry Position . ((+l) *** (+c)) . offsetPair
          start' = posPlus start  $ brkPrefix break
          end'   = posPlus start' $ brkBreak break

offsetPair :: T.Text -> (Int, Int)
offsetPair text
    | T.null text = (0, 0)
    | otherwise   = let lines = T.lines text
                    in  (length lines - 1, T.length (last lines))

offsetWord :: (Maybe PositionRange, Break Word) -> (PositionRange, T.Text)
offsetWord (mpos, break) = (offset break pos, brkBreak break)
    where pos = fromMaybe (PositionRange (Position 0 0) (Position 0 0)) mpos

lookupQuery :: T.Text -> InvertedIndex -> [FileLocation]
lookupQuery query = join . maybeToList . fmap D.toList . M.lookup query

indexTokens :: FilePath -> InvertedIndex -> (PositionRange, T.Text)
            -> InvertedIndex
indexTokens filename iindex (pos, token) =
    M.insertWith mappend token (D.singleton (filename, pos)) iindex

mkContext :: BuilderInfo a -> IO (Maybe (ResultContext a))
mkContext HB{..} = return . Just . uncurry (HC hitPRange hitContext) $
    extractLines hitPRange hitContext hitLines

mkContext (FB _ []) = return Nothing
mkContext (FB fcontext flocs@((fp, _):_)) = do
    lines <- T.lines <$> TIO.readFile (encodeString fp)
    Just . FC fp . catMaybes <$> mapM (mkContext . HB fcontext lines . snd) flocs

mkContext QB{..} = fmap (Just . QC qText . catMaybes)
                 . mapM (mkContext . FB qContext)
                 . L.groupBy onFirsts
                 . L.sort
                 $ lookupQuery qText qIndex

extractLines :: PositionRange -> Int -> [a] -> ((Int, Int), [a])
extractLines (PositionRange start end) context =
    ((startLine, endLine),) . L.take lineRange . L.drop (startLine - 1)
    where startLine = posLine start - context
          endLine   = posLine end   + context
          lineRange = 1 + endLine - startLine

nl :: T.Text
nl = "\n"

underscore' :: Char -> T.Text -> T.Text
underscore' c t = T.replicate (T.length t) $ T.singleton c

formatContext :: ResultContext a -> [T.Text]
formatContext QC{..} =  [ _queryTerm, nl
                        , underscore' '=' _queryTerm, nl, nl
                        ]
                     ++ concatMap formatContext _queryFiles

formatContext FC{..} =  [ contextFile', nl
                        , underscore' '-' contextFile', nl, nl
                        ]
                     ++ concatMap formatContext _fileHits
    where contextFile' = T.pack $ encodeString _contextFile

formatContext HC{..} =  [ tshow _contextLocation, nl
                        , tshow $ fst _contextRange, " - "
                        , tshow $ snd _contextRange, nl
                        ]
                     ++ [ "||| " <> l <> nl | l <- _contextLines
                        ]
                     ++ [nl]

tshow :: Show a => a -> T.Text
tshow = T.pack . show

onFirsts :: Eq a => (a, b) -> (a, c) -> Bool
onFirsts = curry (uncurry (==) . (fst *** fst))

main :: IO ()
main = do
    IX{..} <- execParser indexXmlOpts
    mapM_ TIO.putStr . formatContext . fromMaybe (QC queryText [])
        =<< mkContext . QB queryText contextN
        =<< foldM indexFile M.empty
        =<< listDirectory xmlDir

data IndexXml = IX
              { xmlDir    :: FilePath
              , contextN  :: Int
              , queryText :: T.Text
              } deriving (Show)

indexXmlOpts :: ParserInfo IndexXml
indexXmlOpts = info (helper <*> indexXml)
                    (  fullDesc
                    <> progDesc "Creates an inverted index from XML."
                    <> header "index-xml: Inverted index of XML."
                    )

indexXml :: Parser IndexXml
indexXml =   IX
         <$> xmlDirOption xmlDir
         <*> option (  short 'c'
                    <> long "context"
                    <> metavar "CONTEXT_LINES"
                    <> value 2
                    <> help "The number of context lines before and after\
                            \ each hit. (default = 2)")
        <*> O.argument (Just . T.pack)
                       (metavar "QUERY" <> help "The query to search for.")
    where xmlDir     = "../gk-texts"
