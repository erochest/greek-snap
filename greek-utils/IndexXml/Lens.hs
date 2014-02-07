module IndexXml.Lens
    ( contextLocation
    , contextRange
    , contextStartLine
    , contextEndLine
    , prangeStart
    , prangeEnd
    , pline
    , pcol
    ) where


import           Control.Lens
import           Data.Conduit.Attoparsec (Position (..), PositionRange (..))
import qualified Data.DList              as D

import           IndexXml.Types


contextLocation :: Lens' (ResultContext Hit) (D.DList PositionRange)
contextLocation = lens _contextLocation $ \hc l -> hc { _contextLocation = l }

contextRange :: Lens' (ResultContext Hit) (Int, Int)
contextRange = lens _contextRange $ \hc r -> hc { _contextRange = r }

contextStartLine :: Lens' (ResultContext Hit) Int
contextStartLine = contextRange . _1

contextEndLine :: Lens' (ResultContext Hit) Int
contextEndLine = contextRange . _2

prangeStart :: Lens' PositionRange Position
prangeStart = lens posRangeStart $ \prange s -> prange { posRangeStart = s }

prangeEnd :: Lens' PositionRange Position
prangeEnd = lens posRangeEnd $ \prange s -> prange { posRangeEnd = s }

pline :: Lens' Position Int
pline = lens posLine $ \pos l -> pos { posLine = l }

pcol :: Lens' Position Int
pcol = lens posCol $ \pos c -> pos { posCol = c }

