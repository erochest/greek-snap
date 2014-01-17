{-# LANGUAGE OverloadedStrings #-}


module Text.XML.Utils
    ( foldEvents
    , getText
    ) where


import           Control.Applicative
import           Data.Conduit
import qualified Data.Conduit.List     as CL
import qualified Data.DList            as D
import           Data.Monoid
import qualified Data.Text             as T
import           Data.XML.Types
import           Prelude
import           Text.XML.Stream.Parse


foldEvents :: (a -> Event -> a) -> a -> T.Text -> IO a
foldEvents f a xml = runResourceT $ CL.sourceList [xml]
    $= parseText def
    $= CL.map snd
    $$ CL.fold f a

getText :: T.Text -> IO T.Text
getText xml = T.concat . D.toList <$> foldEvents go D.empty xml
    where go dl (EventContent (ContentText t))   = dl `D.snoc` t
          go dl (EventContent (ContentEntity t)) = dl `D.snoc` ("&" <> t <> ";")
          go dl (EventCDATA t)                   = dl `D.snoc` t
          go dl _                                = dl

