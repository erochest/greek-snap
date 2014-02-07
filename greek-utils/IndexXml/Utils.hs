module IndexXml.Utils
    ( onFirsts
    , onEq
    ) where


import           Control.Arrow


onEq :: Eq b => (a -> b) -> a -> a -> Bool
onEq f = curry (uncurry (==) . (f *** f))

onFirsts :: Eq a => (a, b) -> (a, b) -> Bool
onFirsts = onEq fst
