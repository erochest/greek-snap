{-# LANGUAGE OverloadedStrings #-}


module IndexXml.Opts
    ( indexXmlOpts
    , indexXml
    ) where


import qualified Data.Text           as T
import           Options.Applicative hiding ((&))
import qualified Options.Applicative as O

import           IndexXml.Types
import           Utils


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
