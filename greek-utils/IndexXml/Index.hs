{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}


module IndexXml.Index
    ( indexFile
    , lookupQuery
    , tokenizeText
    ) where


import           Control.Arrow
import           Control.Monad
import qualified Data.Char                 as C
import           Data.Conduit
import           Data.Conduit.Attoparsec   (Position (..), PositionRange (..))
import qualified Data.Conduit.Binary       as CB
import qualified Data.Conduit.List         as CL
import qualified Data.Conduit.Text         as CT
import qualified Data.DList                as D
import qualified Data.HashMap.Strict       as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                 as T
import           Data.Text.ICU
import           Data.XML.Types
import           Filesystem.Path.CurrentOS
import qualified Filesystem.Path.CurrentOS as FS
import           Prelude                   hiding (FilePath)
import           Text.XML.Stream.Parse

import           IndexXml.Types


grc :: LocaleName
grc = Locale "grc"

indexFile :: InvertedIndex -> FilePath -> IO InvertedIndex
indexFile iindex xmlFile = runResourceT $
       CB.sourceFile (FS.encodeString xmlFile)
    $= CT.decode CT.utf8
    $= parseText def
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
