{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -Wall #-}


module XML.Split
    ( Division(..)

    , Split(..)
    , splitId
    , splitPath
    , splitText

    , fromString
    , splitDocument
    ) where


import           Control.Applicative
import           Control.Lens
import           Control.Monad.Identity
import           Data.Conduit
import qualified Data.Conduit.List         as CL
import qualified Data.DList                as D
import qualified Data.List                 as L
import           Data.Monoid
import qualified Data.Text                 as T
import           Data.XML.Types            hiding (Document)
import           Filesystem.Path.CurrentOS
import           Prelude                   hiding (FilePath)
import           Text.XML.Stream.Parse


data Division = Document | Section | Page | Speaking
              deriving (Show, Eq)

type ChunkSize   = Int
type ChunkOffset = Int
type ChunkSpec   = (ChunkSize, ChunkOffset)

data Split = Split
           { _splitPath :: !FilePath
           , _splitId   :: !T.Text
           , _splitText :: !T.Text
           } deriving (Show)
$(makeLenses ''Split)

type Position    = T.Text
type DivisionPos = (Position, T.Text)

data FoldState = FS
               { _fsStack     :: ![Name]
               , _fsIgnoring  :: !Bool
               , _fsDivisions :: !(D.DList DivisionPos)
               , _fsPos       :: !(Maybe Position)
               , _fsCurrent   :: !(D.DList T.Text)
               , _fsInSpeaker :: !Bool
               , _fsSpeaker   :: !(Maybe T.Text)
               }
$(makeLenses ''FoldState)

fromString :: String -> Maybe Division
fromString "document" = Just Document
fromString "section"  = Just Section
fromString "page"     = Just Page
fromString "speaking" = Just Speaking
fromString "sp"       = Just Speaking
fromString _          = Nothing

onElement :: Division -> FoldState -> Event -> FoldState
onElement d fs (EventBeginElement name attrs) = pushElement d fs name attrs
onElement _ fs (EventEndElement name)         = popElement fs name
onElement _ fs (EventContent elContent)
    | _fsInSpeaker fs = fs & fsSpeaker .~ Just (normalize $ contentText elContent)
    | _fsIgnoring fs  = fs
    | otherwise       = addContent fs $ contentText elContent
onElement _ fs (EventCDATA text)
    | _fsInSpeaker fs = fs & fsSpeaker .~ Just (normalize text)
    | _fsIgnoring fs  = fs
    | otherwise       = addContent fs text
onElement _ fs _ = fs

normalize :: T.Text -> T.Text
normalize = T.strip

contentText :: Content -> T.Text
contentText (ContentText t)   = t
contentText (ContentEntity t) = "&" <> t <> ";"

addContent :: FoldState -> T.Text -> FoldState
addContent fs text = fs & over fsCurrent (flip D.snoc text)

currentDivision :: FoldState -> Maybe DivisionPos
currentDivision fs = (,) <$> _fsPos fs <*> current
    where maybeNull dlist = let list = D.toList dlist
                            in  if L.null list
                                    then Nothing
                                    else Just list
          current = fmap T.concat . maybeNull $ _fsCurrent fs

push :: Name -> FoldState -> FoldState
push n = over fsStack (n:)

pop :: FoldState -> FoldState
pop = over fsStack pop'
    where pop' []     = []
          pop' (_:xs) = xs

pushIgnoring :: Name -> FoldState -> FoldState
pushIgnoring n = set fsIgnoring True . push n

popIgnoring :: FoldState -> FoldState
popIgnoring = set fsIgnoring False . pop

attrValue :: Name -> [(Name, [Content])] -> T.Text
attrValue n attrs =
    T.concat
        . map contentText
        $ attrs ^.. traverse . filtered ((== n) . fst) . _2 . traverse

maybeSnoc :: D.DList DivisionPos -> Maybe DivisionPos -> D.DList DivisionPos
maybeSnoc dlist (Just d) = D.snoc dlist d
maybeSnoc dlist Nothing  = dlist

newDivision :: [(Name, [Content])] -> FoldState -> FoldState
newDivision attrs fs =
    let n = attrValue "n" attrs
        c = currentDivision fs
    in  fs & set fsCurrent D.empty
           . set fsPos (Just n)
           . over fsDivisions (flip maybeSnoc c)

pushElement :: Division -> FoldState -> Name -> [(Name, [Content])] -> FoldState
pushElement _ fs n@"head"     _ = fs & pushIgnoring n
pushElement _ fs n@"castList" _ = fs & pushIgnoring n
pushElement _ fs n@"speaker"  _ = fs & pushIgnoring n . set fsInSpeaker True
pushElement d fs "milestone" attrs =
    let atBreak = (== Just d) . fromString . T.unpack $ attrValue "unit" attrs
    in  if atBreak
            then fs & newDivision attrs
            else fs
pushElement _ fs n            _ = fs & push n

popElement :: FoldState -> Name -> FoldState
popElement fs "head"     = fs & popIgnoring
popElement fs "castList" = fs & popIgnoring
popElement fs "speaker"  = fs & popIgnoring . set fsInSpeaker False
popElement fs _            = fs & pop

initFoldState :: FoldState
initFoldState = FS [] False D.empty Nothing D.empty False Nothing

finFoldState :: FoldState -> [DivisionPos]
finFoldState fs = D.toList . maybeSnoc (_fsDivisions fs) $ currentDivision fs

chunk :: ChunkSize -> ChunkOffset -> T.Text -> [T.Text]
chunk size offset = go . T.words
    where go [] = []
          go xs = (T.unwords $ take size xs) : go (drop offset xs)

-- IO here is so we can use ResourceT.
splitDocument :: Division -> ChunkSpec -> FilePath -> T.Text -> IO [Split]
splitDocument division chunkSpec inputPath xmlText =
    fmap (concatMap (split' . fmap chunk') . finFoldState) . runResourceT $
        CL.sourceList [xmlText]
                $= parseText def
                $= CL.map snd
                $$ CL.fold (onElement division) initFoldState
    where chunk' = uncurry chunk chunkSpec
          base   = basename inputPath
          split' = uncurry $ \n -> map (Split base n)

