

module Handler.Split where


import           Control.Error
import qualified Data.Text     as T
import           Import
import           Language.Haskell.TH (Exp(..))


getSplitR :: Handler Html
getSplitR = do
    docs <- fmap (fmap redoc) . runDB $ selectList [] [Asc DocumentTitle]
    defaultLayout $ do
        $(fayFile' (ConE 'StaticR) "Split")
        $(widgetFile "split_form")
    where redoc e = ( either (("ERROR: " <>) . T.pack) id
                    . fromPersistValueText . unKey
                    $ entityKey e
                    , entityVal e
                    )

postSplitR :: Handler Html
postSplitR = undefined

getSplitDownloadR :: Handler TypedContent
getSplitDownloadR = undefined
