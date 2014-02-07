{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS_GHC -Wall #-}

-- TODOS:
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
import           Data.Ord
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

data Couple a = C0
              | C1 a
              | C2 a a

data ResultContext a where
    HC :: { _contextLocation :: D.DList PositionRange
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

contextLocation :: Lens' (ResultContext Hit) (D.DList PositionRange)
contextLocation = lens _contextLocation $ \hc l -> hc { _contextLocation = l }

contextRange :: Lens' (ResultContext Hit) (Int, Int)
contextRange = lens _contextRange $ \hc r -> hc { _contextRange = r }

contextStartLine :: Lens' (ResultContext Hit) Int
contextStartLine = contextRange . _1

contextEndLine :: Lens' (ResultContext Hit) Int
contextEndLine = contextRange . _2

prangeStart :: Lens' PositionRange Position
prangeStart = lens posRangeStart $ \prange s -> prange { posRangeStart = s }

prangeEnd :: Lens' PositionRange Position
prangeEnd = lens posRangeEnd $ \prange s -> prange { posRangeEnd = s }

pline :: Lens' Position Int
pline = lens posLine $ \pos l -> pos { posLine = l }

pcol :: Lens' Position Int
pcol = lens posCol $ \pos c -> pos { posCol = c }

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
mkContext HB{..} = return . Just . uncurry (HC rangeSeq hitContext) $
    extractLines hitPRange hitContext hitLines
    where rangeSeq = D.singleton hitPRange

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

mergeLines :: Int -> [T.Text] -> Int -> [T.Text] -> [T.Text]
mergeLines astart alines bstart blines =
    map snd
        . M.toList
        . M.fromList
        . L.sortBy (comparing fst)
        $ zip [astart..] alines ++ zip [bstart..] blines

combineChildren :: ResultContext a -> ResultContext a
combineChildren a@(HC _ _ _ _)  = a
combineChildren a@(FC _ fhits)  = a { _fileHits   = combineAll fhits }
combineChildren a@(QC _ qfiles) = a { _queryFiles = combineAll qfiles }

combine :: ResultContext a -> ResultContext a -> Couple (ResultContext a)
combine a@(HC _ _ _ _) b@(HC _ _ _ _)
    | astart > bstart = combine b a
    | aend   < bstart = C2 a b
    | otherwise = C1 $ HC ((a ^. contextLocation) <> (b ^. contextLocation))
                          (lend ^. pline - astart)
                          (astart, max aend bend)
                          (mergeLines astart (_contextLines a)
                                      bstart (_contextLines b))
    where lend = D.foldr max (Position 0 0)
               . D.map posRangeEnd
               $ (a ^. contextLocation) <> (b ^. contextLocation)
          (astart, aend) = _contextRange a
          (bstart, bend) = _contextRange b

combine a@(FC afile _) b@(FC bfile _)
    | afile == bfile = walkCombine a b (fst . _contextRange) _fileHits (FC afile)
    | otherwise = C2 (combineChildren a) (combineChildren b)

combine a@(QC aterm _) b@(QC bterm _)
    | aterm == bterm = walkCombine a b _contextFile _queryFiles (QC aterm)
    | otherwise = C2 (combineChildren a) (combineChildren b)

combine _ _ = C0

walkCombine :: Ord b
            => ResultContext a -> ResultContext a
            -> (ResultContext c -> b)
            -> (ResultContext a -> [ResultContext c])
            -> ([ResultContext c] -> ResultContext a)
            -> Couple (ResultContext a)
walkCombine a b sortable childs make =
    C1 . make . combineAll . L.sortBy (comparing sortable)
        $ (childs a) ++ (childs b)

combineAll :: [ResultContext a] -> [ResultContext a]
combineAll []  = []
combineAll [a] = [a]
combineAll (a:b:xs) =
    case combine a b of
        C0     -> combineAll xs
        C1 c   -> combineAll (c : xs)
        C2 c d -> c : combineAll (d : xs)

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

formatContext HC{..} =  L.intersperse "," (map tshow $ D.toList _contextLocation)
                     ++ [ nl
                        , tshow $ fst _contextRange, " - "
                        , tshow $ snd _contextRange, " : "
                        , tshow $ (snd _contextRange) - (fst _contextRange)
                        , nl
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
    mapM_ TIO.putStr
        .   formatContext
        .   combineChildren
        .   fromMaybe (QC queryText [])
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
