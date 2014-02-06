{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS_GHC -Wall #-}


module Main where


import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import qualified Data.Char                 as C
import           Data.Conduit
import           Data.Conduit.Attoparsec   (Position (..), PositionRange (..))
import qualified Data.Conduit.Binary       as CB
import qualified Data.Conduit.List         as CL
import qualified Data.DList                as D
import qualified Data.HashMap.Strict       as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                 as T
import           Data.Text.ICU
import           Data.XML.Types
import           Filesystem
import           Filesystem.Path.CurrentOS hiding (filename)
import qualified Filesystem.Path.CurrentOS as FS
import           Options.Applicative
import           Prelude                   hiding (FilePath, break, lines)
import           Text.XML.Stream.Parse

import           Utils

import           Debug.Trace


type FileLocation  = (FilePath, PositionRange)
type InvertedIndex = M.HashMap T.Text (D.DList FileLocation)

grc :: LocaleName
grc = Locale "grc"

indexFile :: InvertedIndex -> FilePath -> IO InvertedIndex
indexFile index xmlFile = runResourceT $
       CB.sourceFile (FS.encodeString xmlFile)
    $= parseBytesPos def
    $= CL.filter (isContentEvent . snd)
    $= CL.filter (isJust . fst)
    $= CL.map (fmap getText)
    $= CL.concatMap (spread tokenizeText)
    $= CL.map offsetWord
    $$ CL.fold (indexTokens xmlFile) index

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
indexTokens filename index (pos, token) =
    M.insertWith mappend token (D.singleton (filename, pos)) index


main :: IO ()
main = do
    IX{..} <- execParser indexXmlOpts
    index  <- foldM indexFile M.empty =<< listDirectory xmlDir
    undefined

data IndexXml = IX
              { xmlDir       :: FilePath
              , contextLines :: Int
              , queryTerm    :: T.Text
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
        <*> argument (Just . T.pack)
                     (metavar "QUERY" <> help "The query to search for.")
    where xmlDir     = "../gk-texts"
