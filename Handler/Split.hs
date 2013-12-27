

module Handler.Split where


import qualified Data.Text           as T
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
postSplitR = undefined

getSplitDownloadR :: Handler TypedContent
getSplitDownloadR = undefined

data SplitInfo = SI
               { splitDocuments   :: [Entity Document]
               , splitDivision    :: Division
               , splitChunkSize   :: Int
               , splitChunkOffset :: Int
               }

splitFormWidget :: OptionList (Entity Document) -> Widget
splitFormWidget docs = do
    $(whamletFile "templates/split_form.hamlet")

documentOptions :: Handler (OptionList (Entity Document))
documentOptions = optionsPersist [] [Asc DocumentTitle] documentTitle

divisionOptions :: [(T.Text, Division)]
divisionOptions = [ ("document", S.Document)
                  , ("section",  Section)
                  , ("page",     Page)
                  , ("speaking", Speaking)
                  ]

splitForm :: FormInput Handler SplitInfo
splitForm = SI <$> ireq (multiSelectField documentOptions) "document"
               <*> ireq (radioFieldList divisionOptions)   "division"
               <*> ireq intField                           "chunksize"
               <*> ireq intField                           "chunkoffset"

