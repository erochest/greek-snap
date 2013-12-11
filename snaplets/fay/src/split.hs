{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}

module Split where


import           DOM
import           Fay.Text
import qualified Fay.Text as T
import           FFI
import           JQuery
import           Prelude


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
            selectInContext "button" current' >>= onClick (\ev -> do
                JQuery.preventDefault ev
                hide Slow current'
                setTimeout 1 $ const $ doPage next rest'
                return False) >>
            return ()
        [] -> return ()

setProgress :: Int -> Int -> JQuery -> Fay ()
setProgress valueNow valueMax progressBar = do
    setAttr "aria-valuenow" (showt valueNow) progressBar
    setAttr "style" (T.concat ["width: ", showt width, "%;"]) progressBar
    return ()
    where width = truncate $ 100 * fromIntegral valueNow / fromIntegral valueMax

showt :: Show a => a -> Text
showt = pack . show

main :: Fay ()
main = ready $ do
    forM_ fieldSets $ \(_, elid) -> select elid >>= hide Instantly
    case fieldSets of
        (first:rest) -> doPage first rest
        _            -> fail (unpack "ERMAHGERD")
    where fieldSets = [ ("documents", "#split1")
                      , ("divisions", "#split2")
                      , ("chunks",    "#split3")
                      ]

