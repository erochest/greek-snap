{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}


module Handler.Split where


import           Data.Maybe
import           Data.Monoid
import qualified Data.Text           as T
import           Database.Persist
import           Import
import           Language.Haskell.TH (Exp(..))

import           Text.XML.Split
import qualified Text.XML.Split      as S


data Hole = Hole

getSplitR :: Handler Html
getSplitR = do
    docs <- documentOptions
    let formWidget = splitFormWidget docs
    defaultLayout $ do
        $(fayFile' (ConE 'StaticR) "Split")
        $(widgetFile "split")

postSplitR :: Handler Html
postSplitR = do
    urlParams <- splitUrlParams <$> runInputPost splitForm
    defaultLayout $ do
        $(widgetFile "split_done")

getSplitDownloadR :: Handler TypedContent
getSplitDownloadR = undefined

data SplitInfo = SI
               { splitDocuments   :: [Entity Document]
               , splitDivision    :: Division
               , splitChunkSize   :: Int
               , splitChunkOffset :: Int
               }

splitUrlParams :: SplitInfo -> T.Text
splitUrlParams SI{..} =
    mconcat [ "chunksize=",   showt splitChunkSize
            , "&chunkoffset=", showt splitChunkOffset
            , maybe "" (mappend "&division=" . showt)
            $ findi ((== splitDivision) . snd) 1 divisionOptions
            , docParams
            ]
    where docParams   = foldr docp mempty $ mapMaybe intKey splitDocuments
          docp p rest = "&document=" <> showt p <> rest
          showt       = T.pack . show
          intKey (Entity k _) = case unKey k of
                                    PersistInt64 int -> Just int
                                    _                -> Nothing
          findi :: (a -> Bool) -> Int -> [a] -> Maybe Int
          findi _ _ []                 = Nothing
          findi p i (x:xs) | p x       = Just i
                           | otherwise = findi p (i + 1) xs

splitFormWidget :: OptionList (Entity Document) -> Widget
splitFormWidget docs = do
    $(whamletFile "templates/split_form.hamlet")

documentOptions :: Handler (OptionList (Entity Document))
documentOptions = optionsPersist [] [Asc DocumentTitle] documentTitle

divisionOptions :: [(T.Text, Division)]
divisionOptions = [ ("Document", S.Document)
                  , ("Section",  Section)
                  , ("Page",     Page)
                  , ("Speaking", Speaking)
                  ]

splitForm :: FormInput Handler SplitInfo
splitForm = SI <$> ireq (multiSelectField documentOptions) "document"
               <*> ireq (radioFieldList divisionOptions)   "division"
               <*> ireq intField                           "chunksize"
               <*> ireq intField                           "chunkoffset"

