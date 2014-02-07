{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module IndexXml.Output
    ( formatContext
    ) where


import           Data.Conduit.Attoparsec   (Position (..), PositionRange (..))
import qualified Data.DList                as D
import qualified Data.List                 as L
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as TL
import           Data.Text.Lazy.Builder
import           Filesystem.Path.CurrentOS hiding (fromText)

import           IndexXml.Types
import           IndexXml.Utils


hi' :: T.Text
hi' = "*"

hiC :: Builder
hiC = fromText hi'

hi :: T.Text -> T.Text
hi t = mconcat [hi', t, hi']

hib :: T.Text -> Builder
hib t = mconcat [hiChar, fromText t, hiChar]
    where hiChar = fromText hi'

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
                        , tshow $ snd _contextRange - fst _contextRange
                        , nl
                        ]
                     ++ [ "||| " <> l <> nl | l <- lines'
                        ]
                     ++ [nl]
    where lines' = highlight (fst _contextRange) _contextLines
                            $ D.toList _contextLocation

highlight :: Int -> [T.Text] -> [PositionRange] -> [T.Text]
highlight startLine lines = highlight' startLine lines
                          . mapMaybe (addKey key)
                          . L.groupBy (onEq key)
                          . L.sortBy (comparing key)
    where key = posLine . posRangeStart
          addKey _ []       = Nothing
          addKey f xs@(x:_) = Just (f x, xs)

highlight' :: Int -> [T.Text] -> [(Int, [PositionRange])] -> [T.Text]
highlight' _ [] _  = []
highlight' _ ts [] = ts
highlight' n (t:ts) rs@((pn, prs):ranges)
    | n == pn   = highlightLine t prs : highlight' (n + 1) ts ranges
    | otherwise = t : highlight' (n + 1) ts rs

highlightLine :: T.Text -> [PositionRange] -> T.Text
highlightLine line = TL.toStrict
                   . toLazyText
                   . uncurry (mappend . fromText)
                   . foldr hilite (line, mempty)
                   . L.sort
                   . map (pred . posCol)
                   . concatMap (\(PositionRange s e) -> [s, e])
    where by = flip (comparing posCol)
          hilite col (line, b) =
              let (line', chunk) = T.splitAt col line
              in  (line', hiC <> fromText chunk <> b)

tshow :: Show a => a -> T.Text
tshow = T.pack . show

