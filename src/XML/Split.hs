{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS_GHC -Wall #-}


module XML.Split
    ( Division(..)

    , Split(..)
    , splitPath
    , splitId
    , splitN
    , splitText

    , fromString
    , XML.Split.fromText
    , splitDocument
    , splitFile
    ) where


import           Control.Applicative
import           Control.Lens
import           Control.Monad.Identity
import           Data.Conduit
import qualified Data.Conduit.List         as CL
import qualified Data.DList                as D
import qualified Data.HashMap.Strict       as M
import qualified Data.List                 as L
import           Data.Monoid
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import           Data.XML.Types            hiding (Document)
import           Filesystem.Path.CurrentOS
import           Prelude                   hiding (FilePath)
import           Text.XML.Stream.Parse


-- Types and Lenses

data Division = Document | Section | Page | Speaking
              deriving (Show, Eq)

type ChunkSize   = Int
type ChunkOffset = Int
type ChunkSpec   = (ChunkSize, ChunkOffset)

data Split = Split
           { _splitPath :: !FilePath
           , _splitId   :: !T.Text
           , _splitN    :: !Int
           , _splitText :: !T.Text
           } deriving (Show)
$(makeLenses ''Split)

type Position      = T.Text
type DivisionPos   = (Position, T.Text)
type ContentBuffer = D.DList T.Text
type Speaker       = T.Text
type SpeakerIndex  = M.HashMap Speaker ContentBuffer

data FoldState = FS
               { _fsStack        :: ![Name]
               , _fsIgnoring     :: !Bool
               , _fsDivisions    :: !(D.DList DivisionPos)
               , _fsPos          :: !Position
               , _fsCurrent      :: !ContentBuffer
               , _fsInSpeaker    :: !Bool
               , _fsSpeaker      :: !(Maybe Speaker)
               , _fsSpeakerIndex :: !SpeakerIndex
               } deriving (Show)
$(makeLenses ''FoldState)

-- The interface entry point

splitFile :: Division -> ChunkSpec -> FilePath -> IO [Split]
splitFile division chunkSpec inputPath =   TIO.readFile (encodeString inputPath)
                                       >>= splitDocument division chunkSpec inputPath

-- IO here is so we can use ResourceT.
splitDocument :: Division -> ChunkSpec -> FilePath -> T.Text -> IO [Split]
splitDocument division chunkSpec inputPath xmlText =
    fmap ( concatMap (split' . fmap chunk')
         . filter (not . T.null . snd)
         . map (fmap T.strip)
         . finFoldState division
         ) . runResourceT $
        CL.sourceList [xmlText]
                $= parseText def
                $= CL.map snd
                $$ CL.fold (onElement division) initFoldState
    where chunk' = uncurry chunk chunkSpec
          base   = basename inputPath
          split' = uncurry $ \d -> zipWith (Split base d) [0..]

-- Walking over the elements

onElement :: Division -> FoldState -> Event -> FoldState
onElement d fs (EventBeginElement name attrs) = pushElement d fs name attrs
onElement _ fs (EventEndElement name)         = popElement fs name
onElement _ fs (EventContent elContent)       = onContent fs $ contentText elContent
onElement _ fs (EventCDATA text)              = onContent fs text
onElement _ fs _                              = fs

onContent :: FoldState -> T.Text -> FoldState
onContent fs@(FS _ _ _ _ _ True _ _) text = fs & setSpeaker text
onContent fs@(FS _ True _ _ _ _ _ _) _    = fs
onContent fs                         text = fs & addContent text . addSpeakerContent text

pushElement :: Division -> FoldState -> Name -> [(Name, [Content])] -> FoldState
pushElement _ fs n@"head"     _ = fs & pushIgnoring n
pushElement _ fs n@"castList" _ = fs & pushIgnoring n
pushElement _ fs n@"speaker"  _ = fs & pushIgnoring n
                                     . set fsInSpeaker True
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

-- Initializing

initFoldState :: FoldState
initFoldState = FS [] False D.empty "document" D.empty False Nothing M.empty

-- Finalizing

finFoldState :: Division -> FoldState -> [DivisionPos]
finFoldState Speaking fs
    | M.null (_fsSpeakerIndex fs) = finFoldState Document fs
    | otherwise                   = M.toList . fmap (T.concat . D.toList) $ _fsSpeakerIndex fs
finFoldState _ fs                 = D.toList . maybeSnoc (_fsDivisions fs) $ currentDivision fs

chunk :: ChunkSize -> ChunkOffset -> T.Text -> [T.Text]
chunk size offset = go . T.words
    where go [] = []
          go xs = (T.unwords $ take size xs) : go (drop offset xs)

-- Some utilities

fromText :: T.Text -> Maybe Division
fromText "document" = Just Document
fromText "section"  = Just Section
fromText "page"     = Just Page
fromText "speaking" = Just Speaking
fromText "sp"       = Just Speaking
fromText _          = Nothing

fromString :: String -> Maybe Division
fromString "document" = Just Document
fromString "section"  = Just Section
fromString "page"     = Just Page
fromString "speaking" = Just Speaking
fromString "sp"       = Just Speaking
fromString _          = Nothing

normalize :: T.Text -> T.Text
normalize = T.strip

maybeSnoc :: D.DList DivisionPos -> Maybe DivisionPos -> D.DList DivisionPos
maybeSnoc dlist (Just d) = D.snoc dlist d
maybeSnoc dlist Nothing  = dlist

-- XML utilities

attrValue :: Name -> [(Name, [Content])] -> T.Text
attrValue n attrs =
    T.concat
        . map contentText
        $ attrs ^.. traverse . filtered ((== n) . fst) . _2 . traverse

contentText :: Content -> T.Text
contentText (ContentText t)   = t
contentText (ContentEntity t) = "&" <> t <> ";"

-- FoldState utilities

setSpeaker :: Speaker -> FoldState -> FoldState
setSpeaker s = (?~) fsSpeaker (normalize s)
             . over fsSpeakerIndex (`M.union` M.singleton s D.empty)

addContent :: T.Text -> FoldState -> FoldState
addContent text = over fsCurrent (flip D.snoc text)

addSpeakerContent :: T.Text -> FoldState -> FoldState
addSpeakerContent text fs = maybe fs (updateFS text fs) $ _fsSpeaker fs
    where updateFS t fs' s = fs' & over (fsSpeakerIndex . at s) (msnoc t)
          msnoc t mcb      = Just $ maybe (D.singleton t) (`D.snoc` t) mcb

-- | This folds the current division into the list of divisions and returns
-- those.
currentDivision :: FoldState -> Maybe DivisionPos
currentDivision fs = (_fsPos fs,) <$> current
    where current = fmap T.concat . maybeNull $ _fsCurrent fs
          maybeNull dlist = let list = D.toList dlist
                            in  if L.null list
                                    then Nothing
                                    else Just list

-- | This starts a new division by folding the current division into the list
-- of divisions and resetting it.
newDivision :: [(Name, [Content])] -> FoldState -> FoldState
newDivision attrs fs =
    let n = attrValue "n" attrs
        c = currentDivision fs
    in  fs & set fsCurrent D.empty
           . set fsPos n
           . over fsDivisions (flip maybeSnoc c)

-- Stack utilities

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

