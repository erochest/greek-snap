module Utils where


import           Control.Applicative
import           Data.Monoid
import           Filesystem.Path.CurrentOS
import           Options.Applicative
import           Prelude                   hiding (FilePath)
import Data.Text


hole = undefined

data Hole = Hole

fileOption :: Mod OptionFields FilePath -> Parser FilePath
fileOption fields = nullOption (reader (pure . decodeString) <> fields)

textOption :: Mod OptionFields Text -> Parser Text
textOption fields = nullOption (reader (pure . pack) <> fields)

xmlDirOption :: FilePath -> Parser FilePath
xmlDirOption def =
    fileOption (  long "xml-dir"
               <> short 'x'
               <> metavar "XML_DIR"
               <> value def
               <> help (  "The directory containing the XML\
                          \ files to process and load. (Default = '"
                       <> encodeString def <> "'.)"))
