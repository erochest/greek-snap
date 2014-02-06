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
import qualified Data.Text.Lazy            as TL
import qualified Data.Text.Lazy.Builder    as TB
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

data HitContext = HitContext
                { _contextLocation :: FileLocation
                , _contextTerm     :: T.Text
                , _contextCount    :: Int
                , _contextLines    :: [T.Text]
                } deriving (Show)
makeLenses ''HitContext


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

indexTokens :: FilePath -> InvertedIndex -> (PositionRange, T.Text)
            -> InvertedIndex
indexTokens filename iindex (pos, token) =
    M.insertWith mappend token (D.singleton (filename, pos)) iindex

hitContext :: T.Text -> Int -> FileLocation -> [T.Text] -> HitContext
hitContext term context fileLoc =
    HitContext fileLoc term context . extractLines (snd fileLoc) context

extractLines :: PositionRange -> Int -> [a] -> [a]
extractLines (PositionRange start end) context =
    L.take lineRange . L.drop startLine
    where startLine = posLine start - context
          endLine   = posLine end   + context
          lineRange = max 1 $ endLine - startLine

formatContext :: HitContext -> T.Text
formatContext HitContext{..} = TL.toStrict . TB.toLazyText . mconcat $
    [ TB.fromText _contextTerm
    , colon
    , TB.fromString (encodeString filepath)
    , colon
    , TB.fromString (show prange)
    , nl
    ]
    ++ map ((<> nl) . TB.fromText) _contextLines
    where (filepath, prange) = _contextLocation
          nl    = TB.singleton '\n'
          colon = TB.singleton ':'

onFirsts :: Eq a => (a, b) -> (a, c) -> Bool
onFirsts = curry (uncurry (==) . (fst *** fst))

putnl :: IO ()
putnl = putStrLn ""

underscore :: Char -> T.Text -> IO ()
underscore c t =  TIO.putStrLn t
               >> TIO.putStrLn (T.replicate (T.length t) $ T.singleton c)
               >> putnl

printFileGroup :: Int -> T.Text -> [FileLocation] -> IO ()
printFileGroup context query hits@((filepath, _):_) = do
    lines <- T.lines <$> TIO.readFile (encodeString filepath)
    underscore '-' . T.pack $ encodeString filepath
    mapM_ (TIO.putStrLn . formatContext . flip (hitContext query context) lines)
          hits

printFileGroup _     _       []          = return ()

main :: IO ()
main = do
    IX{..} <- execParser indexXmlOpts
    underscore '=' queryTerm
    mapM_ (printFileGroup contextN queryTerm)
        . L.groupBy onFirsts
        . L.sort
        . maybe [] D.toList
        . M.lookup queryTerm
        =<< foldM indexFile M.empty
        =<< listDirectory xmlDir

data IndexXml = IX
              { xmlDir    :: FilePath
              , contextN  :: Int
              , queryTerm :: T.Text
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
