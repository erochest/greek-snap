{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module IndexXml.Output
    ( nl
    , underscore'
    , formatContext
    , tshow
    ) where


import qualified Data.DList                as D
import qualified Data.List                 as L
import           Data.Monoid
import qualified Data.Text                 as T
import           Filesystem.Path.CurrentOS

import           IndexXml.Types


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

