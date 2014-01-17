{-# LANGUAGE OverloadedStrings #-}


module Text.Greek.Tokenize
    ( grc
    , tokenize
    , frequencies
    , stopList
    ) where


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
tokenize = filter (/= " ")
         . map brkBreak
         . filter ((/= Uncategorized) . brkStatus)
         . breaks (breakWord grc)

frequencies :: (Eq a, Hashable a) => [a] -> M.HashMap a Int
frequencies = foldl' inc M.empty

inc :: (Eq a, Hashable a) => M.HashMap a Int -> a -> M.HashMap a Int
inc m t = M.insertWith (+) t 1 m

items :: Int -> a -> Int
items = flip (const succ)

stopList :: [T.Text] -> [(T.Text, Double)]
stopList = uncurry ratioize
         . fmap (sortBy (flip (comparing snd)) . M.toList)
         . foldl' (foldPair items inc) (0, M.empty)
         . concatMap tokenize
    where ratioize total xs = let total' = double total
                              in  map (fmap ((/ total') . double)) xs

double :: Int -> Double
double = fromRational . fromIntegral

foldPair :: (a -> c -> a) -> (b -> c -> b) -> (a, b) -> c -> (a, b)
foldPair f g (a, b) c = (f a c, g b c)

