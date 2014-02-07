{-# LANGUAGE GADTs #-}


module IndexXml.Types
    ( FileLocation
    , InvertedIndex
    , Hit
    , File
    , Query
    , Couple(..)
    , ResultContext(..)
    , BuilderInfo(..)
    , IndexXml(..)
    ) where


import           Data.Conduit.Attoparsec   (Position (..), PositionRange (..))
import qualified Data.DList                as D
import qualified Data.HashMap.Strict       as M
import qualified Data.Text                 as T
import           Filesystem.Path.CurrentOS
import           Prelude                   hiding (FilePath)


type FileLocation  = (FilePath, PositionRange)
type InvertedIndex = M.HashMap T.Text (D.DList FileLocation)

data Hit
data File
data Query

data Couple a = C0
              | C1 a
              | C2 a a

data ResultContext a where
    HC :: { _contextLocation :: D.DList PositionRange
          , _contextCount    :: Int
          , _contextRange    :: (Int, Int)
          , _contextLines    :: [T.Text]
          }                                -> ResultContext Hit
    FC :: { _contextFile :: FilePath
          , _fileHits    :: [ResultContext Hit]
          }                                -> ResultContext File
    QC :: { _queryTerm  :: T.Text
          , _queryFiles :: [ResultContext File]
          }                                -> ResultContext Query

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

data IndexXml = IX
              { xmlDir    :: FilePath
              , contextN  :: Int
              , queryText :: T.Text
              } deriving (Show)
