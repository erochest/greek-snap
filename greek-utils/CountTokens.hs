{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main where


import           Control.Monad
import qualified Data.DList                as D
import qualified Data.HashMap.Strict       as M
import qualified Data.List                 as L
import           Data.Ord
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import           Filesystem
import           Filesystem.Path.CurrentOS
import           Options.Applicative
import           Prelude                   hiding (FilePath, break, lines)

import           IndexXml.Index
import           IndexXml.Types
import           Utils


formatPair :: (T.Text, Int) -> T.Text
formatPair (t, i) = t <> "\t" <> T.pack (show i)

countHits :: D.DList a -> Int
countHits = length . D.toList

sortFreqs :: [(T.Text, Int)] -> [(T.Text, Int)]
sortFreqs = L.sortBy (flip (comparing snd))

indexFreqs :: InvertedIndex -> [(T.Text, Int)]
indexFreqs = sortFreqs . map (fmap countHits) . M.toList

main :: IO ()
main =   Main.xmlDir <$> execParser countTokensOpts
     >>= listDirectory
     >>= fmap indexFreqs . foldM indexFile M.empty
     >>= mapM_ (TIO.putStrLn . formatPair)

data CountTokens = CT
                 { xmlDir :: FilePath
                 } deriving (Show)

countTokensOpts :: ParserInfo CountTokens
countTokensOpts = info (helper <*> countTokens)
                       (  fullDesc
                       <> progDesc "Dump the token frequencies as TSV."
                       <> header "count-tokens: Dumps token frequencies."
                       )

countTokens :: Parser CountTokens
countTokens = CT <$> xmlDirOption "../gk-texts"
