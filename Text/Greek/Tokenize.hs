{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}


module Text.Greek.Tokenize
    ( grc
    , tokenize
    , frequencies
    , stopListRaw
    , stopListMerge
    , stopListRatios
    , stopList
    ) where


import           Data.Char           (isSpace)
import           Data.Hashable
import qualified Data.HashMap.Strict as M
import           Data.List           (foldl')
import           Data.List           (sortBy)
import           Data.Ord
import qualified Data.Text           as T
import           Data.Text.ICU
import           Import


grc :: LocaleName
grc = Locale "grc"

tokenize :: T.Text -> [T.Text]
tokenize input = let !tokens = concatMap tokenizeLine $ T.lines input
                 in  tokens
    where tokenizeLine = filter (not . T.all isSpace)
                       . map brkBreak
                       . filter ((/= Uncategorized) . brkStatus)
                       . breaks (breakWord grc)

frequencies :: (Eq a, Hashable a) => [a] -> M.HashMap a Int
frequencies = foldl' inc M.empty

inc :: (Eq a, Hashable a) => M.HashMap a Int -> a -> M.HashMap a Int
inc m t = M.insertWith (+) t 1 m

items :: Int -> a -> Int
items = flip (const succ)

stopListRaw :: T.Text -> (Int, M.HashMap T.Text Int)
stopListRaw = foldl' (foldPair items inc) (0, M.empty) . tokenize

stopListMerge :: (Eq a, Hashable a)
              => [(Int, M.HashMap a Int)] -> (Int, M.HashMap a Int)
stopListMerge = foldl' (step (+) (M.unionWith (+))) (0, M.empty)
    where step f g (a, b) (c, d) = (f a c, g b d)

stopListRatios :: (Int, M.HashMap T.Text Int) -> [(T.Text, Double)]
stopListRatios (total, index) = sortBy descSnd
                              . M.toList
                              $ M.map ((/ total') . double) index
    where total'  = double total
          descSnd = flip (comparing snd)

stopList :: [T.Text] -> [(T.Text, Double)]
stopList = stopListRatios . stopListMerge . map stopListRaw

double :: Int -> Double
double = fromRational . fromIntegral

foldPair :: (a -> c -> a) -> (b -> c -> b) -> (a, b) -> c -> (a, b)
foldPair f g (a, b) c =
    let !f' = f a c
        !g' = g b c
    in  (f', g')

