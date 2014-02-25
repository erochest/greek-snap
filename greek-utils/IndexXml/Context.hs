{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}


module IndexXml.Context
    ( makeContext
    , combineChildren
    ) where


import           Control.Applicative
import           Control.Lens
import           Data.Conduit.Attoparsec   (Position (..), PositionRange (..))
import qualified Data.DList                as D
import qualified Data.HashMap.Strict       as M
import qualified Data.List                 as L
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import           Filesystem.Path.CurrentOS

import           IndexXml.Index
import           IndexXml.Lens
import           IndexXml.Types
import           IndexXml.Utils


data Couple a = C0
              | C1 a
              | C2 a a

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

makeContext :: ContentGetter -> T.Text -> Int -> InvertedIndex -> IO (Maybe (ResultContext Query))
makeContext getter term context iindex = mkContext getter $ QB term context iindex

mkContext :: ContentGetter -> BuilderInfo a -> IO (Maybe (ResultContext a))
mkContext getter HB{..} = return . Just . uncurry (HC rangeSeq hitContext) $
    extractLines hitPRange hitContext hitLines
    where rangeSeq = D.singleton hitPRange

mkContext _ (FB _ []) = return Nothing
mkContext getter (FB fcontext flocs@((fp, _):_)) = do
    lines <- T.lines <$> getter fp
    Just . FC fp . catMaybes <$>
            mapM (mkContext getter . HB fcontext lines . snd) flocs

mkContext getter QB{..} =
      fmap (Just . QC qText . catMaybes)
    . mapM (mkContext getter . FB qContext)
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
combineChildren a@HC{}          = a
combineChildren a@(FC _ fhits)  = a { _fileHits   = combineAll fhits }
combineChildren a@(QC _ qfiles) = a { _queryFiles = combineAll qfiles }

combine :: ResultContext a -> ResultContext a -> Couple (ResultContext a)
combine a@HC{} b@HC{}
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
        $ childs a ++ childs b

combineAll :: [ResultContext a] -> [ResultContext a]
combineAll []  = []
combineAll [a] = [a]
combineAll (a:b:xs) =
    case combine a b of
        C0     -> combineAll xs
        C1 c   -> combineAll (c : xs)
        C2 c d -> c : combineAll (d : xs)
