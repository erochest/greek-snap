{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}


module Handler.StopList where


import           Control.Monad.Trans.Writer (Writer)
import           Data.Char
import qualified Data.List                  as L
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import           Data.Text.Lazy.Builder
import           Data.Text.Lazy.Builder.Int
import           Import
import           Text.Greek.Tokenize
import           Text.XML.Utils


data StopListSpec = StopList
                  { stopListN    :: Maybe Int
                  , stopListP    :: Maybe Double
                  }

toParams :: StopListSpec -> T.Text
toParams StopList{..} =
    T.intercalate "&" $ catMaybes [ Just "_hasdata="
                                  , addp "f2=" stopListN
                                  , addp "f3=" stopListP
                                  ]
    where addp name = fmap (T.append name . T.pack . show)

getStopListR :: Handler TypedContent
getStopListR = do
    ((result, widget), encType) <- runFormGet $ renderBootstrap stopListAForm
    case result of
        FormFailure errs -> selectRep $ showErrors result widget encType errs
        FormMissing      -> selectRep $ provideRep $ showForm result widget encType
        FormSuccess spec -> selectRep . showStopList spec =<< getStopList spec

showErrors :: FormResult StopListSpec -> Widget -> Enctype -> [Text]
           -> Writer (Endo [ProvidedRep Handler]) ()
showErrors result widget encType errs = do
    provideRep $ showForm result widget encType
    provideRep $ return $ object [ "status"   .= ("error" :: T.Text)
                                 , "messages" .= errs
                                 ]
    provideRep $ return $ T.intercalate "\n" ("ERRORS:" : errs)

showForm :: FormResult StopListSpec -> Widget -> Enctype -> Handler Html
showForm result widget encType = defaultLayout $ do
    setTitle "Stop List"
    $(widgetFile "stop_list_form")

showStopList :: StopListSpec -> [(T.Text, Double)] -> Writer (Endo [ProvidedRep Handler]) ()
showStopList spec stops = do
    provideRep $ defaultLayout $ do
        let stopList' = showCodes $ fullStopList stops
        setTitle "Stop List"
        let params = toParams spec
        $(widgetFile "stop_list")
    provideRep $ stopListText stops
    provideRep $ stopListJson stops
    where addp name = fmap (T.append name . T.pack . show)

altHandler :: ([(T.Text, Double)] -> Handler a) -> Handler a
altHandler h = do
    ((result, _), _) <- runFormGet $ renderBootstrap stopListAForm
    case result of
        FormSuccess spec -> h =<< getStopList spec
        _                -> redirect . (StopListR,) . reqGetParams =<< getRequest

getStopListJsonR :: Handler RepJson
getStopListJsonR = altHandler stopListJson

stopListJson :: [(T.Text, Double)] -> Handler RepJson
stopListJson = return . repJson . array . map toObj . fullStopList
    where toObj (n, t, c, f) = object [ "n"     .= n
                                      , "token" .= t
                                      , "codes" .= c
                                      , "freq"  .= f
                                      ]

fullStopList :: [(T.Text, Double)] -> [(Int, T.Text, [Int], Double)]
fullStopList = map decorate . zip [1..]
    where codes = map ord . T.unpack
          decorate (i, (t, f)) = (i, t, codes t, f)

showCodes :: [(Int, T.Text, [Int], Double)] -> [(Int, T.Text, T.Text, Double)]
showCodes = map (onThird showCodes')
    where onThird f (a, b, c, d) = (a, b, f c, d)
          showCodes' = TL.toStrict
                     . toLazyText
                     . foldr mappend mempty
                     . L.intersperse (singleton ' ')
                     . map (mappend "U+" . hexadecimal)

getStopListTextR :: Handler RepPlain
getStopListTextR = altHandler stopListText

stopListText :: [(T.Text, Double)] -> Handler RepPlain
stopListText = return . repPlain . T.append "\n" . T.intercalate "\n" . map fst

maybeapp :: Maybe (a -> a) -> a -> a
maybeapp mf = fromMaybe id mf

getStopList :: StopListSpec -> Handler [(T.Text, Double)]
getStopList StopList{..} = do
    texts <-  mapM (liftIO . getText . documentContent . entityVal)
          =<< runDB (selectList [] [])
    return . maybeapp (fmap dropWhile . fmap (flip ((>) . snd)) $ stopListP)
           . maybeapp (take <$> stopListN)
           $ stopList texts

stopListAForm :: AForm Handler StopListSpec
stopListAForm =   StopList
              <$> aopt intField "Maximum Items" Nothing
              <*> aopt doubleField "Cut off frequency percentage (blank to ignore)" Nothing

