{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}


module Split where


import           DOM
import           Fay.Text
import qualified Fay.Text as T
import           FFI
import           JQuery
import           Language.Fay.Yesod
import           Prelude
import           SharedTypes


log' :: Text -> Fay ()
log' = ffi "console['log'](%1)"

hideAll :: Speed -> [JQuery] -> Fay ()
hideAll spd = mapM_ (hide spd)

mapM :: (a -> Fay b) -> [a] -> Fay [b]
mapM _ []     = return []
mapM f (x:xs) = do
    y  <- f x
    ys <- mapM f xs
    return $ y:ys

doPage :: (Text, Text) -> [(Text, Text)] -> Fay ()
doPage current@(subtitle, elid) rest = do
    current' <- select elid
    setText subtitle =<< select "#subtitle"
    setProgress (2 - Prelude.length rest) 3 =<< select ".progress-bar"
    jshow Slow current'
    case rest of
        (next:rest') ->
            selectInContext "button.next" current' >>= onClick (\ev -> do
                JQuery.preventDefault ev
                hide Slow current'
                setTimeout 1 $ const $ doPage next rest'
                return False) >>
            return ()
        [] -> return ()

setProgress :: Int -> Int -> JQuery -> Fay ()
setProgress valueNow valueMax progressBar = do
    JQuery.setAttr "aria-valuenow" (showt valueNow) progressBar
    JQuery.setAttr "style" (T.concat ["width: ", showt width, "%;"]) progressBar
    return ()
    where width = truncate $ 100 * fromIntegral valueNow / fromIntegral valueMax

setSelected :: Bool -> JQuery.Element -> Fay ()
setSelected = ffi "%2['selected'] = %1"

addSelectAll :: Fay ()
addSelectAll =
        select "#selectall"
    >>= onClick (\ev ->
                    JQuery.preventDefault ev
                >>  select "#document option"
                >>= each (\_ el -> setSelected True el >> return True)
                >>  return False)
    >>  return ()

showt :: Show a => a -> Text
showt = pack . show

main :: Fay ()
main = ready $ do
    addSelectAll
    case fieldSets of
        (first:rest) -> doPage first rest
        _            -> fail (unpack "ERMAHGERD")
    where fieldSets = [ ("documents", "#split1")
                      , ("divisions", "#split2")
                      , ("chunks",    "#split3")
                      ]

