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
import           Filesystem.Path.CurrentOS hiding (filename)
import qualified Filesystem.Path.CurrentOS as FS
import           Options.Applicative       hiding ((&))
import qualified Options.Applicative       as O
import           Prelude                   hiding (FilePath, break, lines)
import           Text.XML.Stream.Parse

import           Utils

import           Debug.Trace


type FileLocation  = (FilePath, PositionRange)
type InvertedIndex = M.HashMap T.Text (D.DList FileLocation)

data HitContext = HC
                { _contextLocation :: PositionRange
                , _contextCount    :: Int
                , _contextRange    :: (Int, Int)
                , _contextLines    :: [T.Text]
                } deriving (Show)
makeLenses ''HitContext

data FileContext = FC
                 { _contextFile :: FilePath
                 , _fileHits :: [HitContext]
                 } deriving (Show)
makeLenses ''FileContext

data QueryContext = QC
                  { _queryTerm :: T.Text
                  , _queryFiles :: [FileContext]
                  } deriving (Show)
makeLenses ''QueryContext


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

hitContext :: Int -> [T.Text] -> PositionRange -> HitContext
hitContext context lines prange =
    uncurry (HC prange context) $ extractLines prange context lines

fileContext :: Int -> [FileLocation] -> IO (Maybe FileContext)
fileContext _ [] = return Nothing
fileContext context ps@((fp, _):_) = do
    lines <- T.lines <$> TIO.readFile (encodeString fp)
    return . Just . FC fp $ map (hitContext context lines . snd) ps

queryContext :: T.Text -> Int -> InvertedIndex -> IO QueryContext
queryContext query context iindex =
    fmap (QC query . catMaybes)
        . mapM (fileContext context)
        . L.groupBy onFirsts
        . L.sort
        $ lookupQuery query iindex

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

formatQuery :: QueryContext -> [T.Text]
formatQuery QC{..} =  [ _queryTerm, nl
                      , underscore' '=' _queryTerm, nl, nl
                      ]
                   ++ concatMap formatFile _queryFiles

formatFile :: FileContext -> [T.Text]
formatFile FC{..} =  [ contextFile', nl
                     , underscore' '-' contextFile', nl, nl
                     ]
                  ++ concatMap formatHit _fileHits
    where contextFile' = T.pack $ encodeString _contextFile

formatHit :: HitContext -> [T.Text]
formatHit HC{..} =  [ tshow _contextLocation, nl
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
    mapM_ TIO.putStr . formatQuery
        =<< queryContext queryText contextN
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
