module IndexXml.Utils
    ( onFirsts
    ) where


import           Control.Arrow


onFirsts :: Eq a => (a, b) -> (a, c) -> Bool
onFirsts = curry (uncurry (==) . (fst *** fst))

