{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS_GHC -Wall #-}

-- TODOS:
-- [ ] highlighting
-- [ ] highlighting options


module Main where


import           Control.Monad
import qualified Data.HashMap.Strict       as M
import           Data.Maybe
import qualified Data.Text.IO              as TIO
import           Filesystem
import           Options.Applicative
import           Prelude                   hiding (FilePath, break, lines)

import           IndexXml.Context
import           IndexXml.Index
import           IndexXml.Opts
import           IndexXml.Output
import           IndexXml.Types

import           Debug.Trace


main :: IO ()
main = do
    IX{..} <- execParser indexXmlOpts
    mapM_ TIO.putStr
        .   formatContext
        .   combineChildren
        .   fromMaybe (QC queryText [])
        =<< mkContext . QB queryText contextN
        =<< foldM indexFile M.empty
        =<< listDirectory xmlDir

