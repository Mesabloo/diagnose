{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns     #-}
module Error.Diagnose.Layout.CodeSpanReporting.Render where

import Error.Diagnose.Layout.CodeSpanReporting.Config

import qualified Data.Array.IArray as A (Array, array, bounds, (!))
import qualified Data.HashMap.Lazy as H ((!?))
import qualified Data.List.NonEmpty as N (cons, head, singleton, toList)
import qualified Data.Text as T (length, split)

import Control.Arrow ((&&&))
import Data.Array.Unboxed (UArray)
import Data.Bifunctor (bimap, first, second)
import Data.Char (GeneralCategory (Control), generalCategory, isSpace, ord)
import Data.Char.WCWidth (wcwidth)
import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.List (dropWhileEnd, groupBy, intersperse, nub, sort, sortOn, uncons, unfoldr)
import Data.Maybe (fromJust, isJust, isNothing, mapMaybe)
import Data.Ord (comparing)
import Text.Printf (printf)

import qualified Error.Diagnose as E (Marker (..), Note (..), Severity (..))

import Error.Diagnose (IsAnnotation (mkColor), MarkerKind (..), Report (..), SourceRange (..), align, hsep)
import Error.Diagnose.Layout (FileMap)
import Prettyprinter (Doc, Pretty (..), annotate, brackets, colon, column, emptyDoc, fill, hardline, space, (<+>))
import Prettyprinter.Internal (Doc (..))
import Prettyprinter.Render.Terminal (Color (..), bold, color, colorDull)

unicodeWidth :: Int -> Int -> Char -> Int
unicodeWidth tabSize col c@(wcwidth -> w)
  | w >= 0    = w
  | c == '\t' = (col `div` tabSize + 1) * tabSize - col
  | otherwise = error (printf "negative width for '%c' (0x%04x)" c (ord c))

simpleWidth :: Char -> Int
simpleWidth c@(wcwidth -> w)
  | w >= 0    = w
  | c == '\t' = error "simpleWidth: cannot handle tab character"
  | otherwise = error (printf "negative width for '%c' (0x%04x)" c (ord c))

data Severity
  = Bug
  | Error
  | Warning
  | Help
  | Note
  deriving (Show, Eq, Ord)

instance Pretty Severity where
  pretty Bug     = "bug"
  pretty Error   = "error"
  pretty Warning = "warning"
  pretty Help    = "help"
  pretty Note    = "note"

data Annotation
  = Header Severity
  | HeaderMessage
  | SourceBorder
  | NoteBullet
  | LineNumber
  | SourceTint Severity MarkerStyle
  | MarkerTint Severity MarkerStyle
  deriving (Show, Eq)

instance IsAnnotation Annotation where
  mkColor = \case
    Header Bug         -> bold <> color Red
    Header Error       -> bold <> color Red
    Header Warning     -> bold <> color Yellow
    Header Note        -> bold <> color Green
    Header Help        -> bold <> color Cyan
    HeaderMessage      -> bold <> color White
    SourceBorder       -> colorDull Cyan -- Blue
    NoteBullet         -> colorDull Cyan -- Blue
    LineNumber         -> colorDull Cyan -- Blue
    SourceTint sev sty -> marker sev sty True
    MarkerTint sev sty -> marker sev sty False
    where marker Bug     SPrimary   _       = colorDull Red
          marker Error   SPrimary   _       = colorDull Red
          marker Warning SPrimary   _       = colorDull Yellow
          marker Note    SPrimary   _       = colorDull Green
          marker Help    SPrimary   _       = colorDull Cyan
          marker _         SBlank     _     = mempty
          marker _         SAdd       _     = color Green
          marker _         SRemove    _     = color Red
          marker _         SSecondary True  = colorDull White
          marker _         SSecondary False = colorDull Cyan -- Blue

data MarkerStyle
  = SAdd
  | SRemove
  | SPrimary
  | SSecondary
  | SBlank
  deriving (Show, Eq, Ord)

data Marker msg = Marker
  { markerStyle     :: MarkerStyle
  , markerMessage   :: Maybe msg
  , markerInsertion :: Maybe String
  } deriving (Show)

nonBlank :: [(a, Marker msg)] -> [(a, Marker msg)]
nonBlank = filter ((/= SBlank) . markerStyle . snd)

takeNAndOthers :: Pretty a => Int -> [a] -> Doc ann
takeNAndOthers 0 _           = error "takeNAndOthers: cannot take 0"
takeNAndOthers _ []          = error "takeNAndOthers: empty list"
takeNAndOthers n (x0 : rest) = pretty x0 <> go (pred n) rest
  where go _ []       = emptyDoc
        go 0 [x]      = ", and " <> pretty x
        go 0 others   = ", and " <> pretty (length others) <> " other(s)"
        go k (x : xs) = ", " <> pretty x <> go (pred k) xs

{-
  Basic Common Types
  ==================

  These should be self-documenting.
  Note that 'Line', 'Colomn', 'Range', etc. are all type synonyms only.
-}

type Line = Int
type Column = Int
type Range a = (a, a)

mapRange :: (a -> b) -> Range a -> Range b
mapRange f = bimap f f

inRange :: Ord a => a -> Range a -> Bool
x `inRange` (l, r) = l <= x && x <= r

isOverlapping :: Ord a => Range a -> Range a -> Bool
isOverlapping (l1, r1) (l2, r2) = r1 >= l2 && r2 >= l1

combineRange :: Ord a => Range a -> Range a -> Range a
combineRange (l1, r1) (l2, r2) = (min l1 l2, max r1 r2)

type SingleMarker msg = (Range Column, Marker msg)
type MultiMarker msg = (Range (Line, Column), Marker msg)

linesForMulti :: MultiMarker msg -> [Line]
linesForMulti (((lnS, _), (lnE, _)), _) = [lnS, lnE]

linesForMultis :: [MultiMarker msg] -> [Line]
linesForMultis = nub . sort . concatMap linesForMulti

{-
  Overall Process
  ===============

  1. Classify single-line and multi-line markers

  - Single-line markers: @(Line, (Range Column, Marker msg))@
  - Multi-line markers: @(Range (Line, Column), Marker msg)@

  2. Group multi-line markers into disjoint groups

  - @(_, (ln1E, _))@ and @((ln2S, _), _)@ are "disjoint" iff @ln1E < ln2S@
  - Each group of multi-line markers can be laid out separately

  3. Extract lines of interest

  - start and end lines of multi-line markers
  - lines of single-line markers
  - padding lines: existence of @n@ and @n + 2@ implies existence of @n@

  4. Associate each line with its single-line markers and multi-line markers in its group (determined in 2)

  5. Render each line with the associated markers

  6. Render notes and helps (no markers: inline; with markers: sub-report)
-}

data GenReport msg = GenReport
  { reportSeverity   :: Severity
  , reportErrorCode  :: Maybe msg
  , reportMessage    :: msg
  , reportMarkers    :: [(SourceRange, Marker msg)]
  , reportNotes      :: [(Severity, msg)]
  , reportSubReports :: [GenReport msg]
  }

reportToGenReport :: Report msg -> GenReport msg
reportToGenReport (Report sev reportErrorCode reportMessage markers notes) = GenReport{..}
  where reportSeverity = case sev of
          E.Warning  -> Warning
          E.Error    -> Error
          E.Critical -> Bug
        reportMarkers = map mainMarkerToMarker markers
        classifyNote (noteSev, msg, mm)
          | Just m <- mm = Right (noteToGenReport noteSev msg m)
          | otherwise = Left (noteSev, msg)
        noteToTriple (E.Note msg ms) = (Note, msg, ms)
        noteToTriple (E.Hint msg ms) = (Help, msg, ms)
        (reportNotes, reportSubReports) = partitionEither (classifyNote . noteToTriple) notes

mainMarkerToMarker :: E.Marker msg 'MainMarker -> (SourceRange, Marker msg)
mainMarkerToMarker (E.Primary range msg)   = (range, Marker SPrimary msg Nothing)
mainMarkerToMarker (E.Secondary range msg) = (range, Marker SSecondary msg Nothing)
mainMarkerToMarker (E.Blank range)         = (range, Marker SBlank Nothing Nothing)

noteToGenReport :: Severity -> msg -> E.Marker msg 'NoteMarker -> GenReport msg
noteToGenReport reportSeverity reportMessage (pure . noteMarkerToMarker -> reportMarkers)
  = GenReport{reportErrorCode = Nothing, reportNotes = [], reportSubReports = [], ..}

noteMarkerToMarker :: E.Marker msg 'NoteMarker -> (SourceRange, Marker msg)
noteMarkerToMarker (E.AddCode begin@(l, c) file len text)
  = (Range{ file, begin, end = (l, c + len - 1) }, Marker SAdd Nothing (Just text))
noteMarkerToMarker (E.RemoveCode range)   = (range, Marker SRemove Nothing Nothing)
noteMarkerToMarker (E.Annotate range msg) = (range, Marker SSecondary msg Nothing)

report :: Pretty msg => FileMap -> Chars -> Int -> Report msg -> Doc Annotation
report fileMap chars tabSize = genReport fileMap chars tabSize . reportToGenReport

genReport :: Pretty msg => FileMap -> Chars -> Int -> GenReport msg -> Doc Annotation
genReport fileMap chars@Chars{ cSourceBorderLeft } tabSize
  GenReport{ reportSeverity = sev, reportErrorCode = code, reportMessage = msg
           , reportMarkers = markers, reportNotes = notes, reportSubReports = subReports }
  = header sev code msg <> foldMap renderFile groups
  <> foldMap (renderInlineNote chars maxLnWidth) notes
  <> foldMap (genReport fileMap chars tabSize) subReports
  where groups = sortMarkers markers
        maxLnWidth = length $ show $ maximum $ 0 : concatMap go markers
          where go (Range{ begin, end }, _) = [fst begin, snd end]
        leftPadding = pad maxLnWidth ""
        trailingLeftBorder = leftPadding <+> annotate SourceBorder (pretty cSourceBorderLeft) <> hardline
        sortMarkers
          = map (file . fst . head &&& id)
          . groupBy ((==) `on` file . fst)
          . sortOn (posToTriple . fst)
        posToTriple Range{ begin, end, file } = (file, begin, end)
        renderFile (fileName, thisMarkers)
          | Just fileLines <- fileMap H.!? fileName
          , let (singles, multis) = classifyAndGroupMarkers fileLines thisMarkers
          , let markedLines = linesOfInterest fileLines singles multis
          , let maxMultiCount = maximum (0 : map (length . nonBlank . snd) multis)
          , let missingLines = filter (not . (`inRange` A.bounds fileLines) . pred) allLines
          , let go = sourceLine chars tabSize maxLnWidth maxMultiCount sev
          = if null missingLines
          then snippetStart chars maxLnWidth startPos <> foldMap go markedLines <> trailingLeftBorder
          else makeBug ("line " <> takeNAndOthers 2 missingLines <> " of file '" <> pretty fileName <> "' not available")
          | otherwise = makeBug ("content of file '" <> pretty fileName <> "' not available")
          where allLines = nub $ sort $ concatMap (\(Range{..}, _) -> [fst begin, fst end]) thisMarkers
                startPos = fst (head thisMarkers)
                makeBug s = leftPadding <+> annotate (Header Bug) "bug" <> annotate HeaderMessage (colon <+> s) <> hardline

partitionEither :: (a -> Either b c) -> [a] -> ([b], [c])
partitionEither p = foldr go ([], [])
  where go (p -> Left b)  ~(bs, cs) = (b : bs, cs)
        go (p -> Right c) ~(bs, cs) = (bs, c : cs)

-- | 1. Classify single-line and multi-line markers
classifyMarkers :: [(SourceRange, Marker msg)] -> ([(Line, SingleMarker msg)], [MultiMarker msg])
classifyMarkers = partitionEither \(pos, marker) ->
  let Range{ begin = begin@(lnS, colS), end = end@(lnE, colE) } = pos
  in if lnS == lnE then Left (lnS, ((colS, colE), marker)) else Right ((begin, end), marker)

classifyAndGroupMarkers :: A.Array Int String -> [(SourceRange, Marker msg)] -> ([(Line, [SingleMarker msg])], [MultiGroup msg])
classifyAndGroupMarkers fileLines = bimap groupSingles (groupMultis fileLines) . classifyMarkers

-- | 2. Group multi-line markers into disjoint groups
--
--   Note: The input is expected to be sorted, but we cannot use 'groupBy' because it gives incorrect semantics.
groupMultis :: A.Array Int String -> [MultiMarker msg] -> [MultiGroup msg]
groupMultis fileLines = map (second N.toList) . foldr combine [] . scanlAndLabel label (-1) -- go (-1)
  where label maxSoFar this = maxSoFar `max` endLine this
        combine (_,    this) [] = [(lineRange this, N.singleton this)]
        combine (maxE, this) res@((rng, g) : rest)
          -- decide: whether group [.. this] overlaps with the next one
          -- overlapping, push 'this' onto the group (starting with 'this')
          | lnS < maxE || (lnS == maxE && colS <= leadingSpaces)
            = (combineRange thisRng rng, N.cons this g) : rest
          -- not overlapping, start a new group (ending with 'this')
          -- note: we have already sorted the markers,
          | otherwise = (thisRng, N.singleton this) : res
          where thisRng = lineRange this
                lnS = startLine (N.head g)
                colS = startCol (N.head g)
                text = fileLines A.! pred lnS
                leadingSpaces = length (takeWhile isSpace text)
        lineRange (((lnS, _), (lnE, _)), _) = (lnS, lnE)
        startCol (((_, colS), _), _) = colS
        startLine = fst . lineRange
        endLine = snd . lineRange

scanlAndLabel :: (b -> a -> b) -> b -> [a] -> [(b, a)]
scanlAndLabel f e0 = unfoldr go . (e0, )
  where go (_, [])     = Nothing
        go (e, x : xs) = let e' = f e x in Just ((e', x), (e', xs))

type MultiGroup msg = (Range Line, [MultiMarker msg])

linesForMultiGroups :: [MultiGroup msg] -> [Line]
linesForMultiGroups = nub . concatMap (nub . sort . concatMap linesForMulti . snd)

groupSingles :: [(Line, SingleMarker msg)] -> [(Line, [SingleMarker msg])]
groupSingles = map (fst . head &&& map snd) . groupBy ((==) `on` fst)

-- | 3. Extract lines of interest
--   4. Associate each line with its single-line markers and multi-line markers in its group (determined in 2)
--
--   note: 'error' if there are missing lines. Check before use.
linesOfInterest :: A.Array Int String -> [(Line, [SingleMarker msg])] -> [MultiGroup msg] -> [MarkedLine msg]
linesOfInterest fileLines singles multiGroups = unfoldr go (theLines, multiGroups)
  where theLines = fillGap (mergeMarkers singles (linesForMultiGroups multiGroups))
        go ([], _) = Nothing
        go (ls, []) = go (ls, [((maxBound, maxBound), [])])
        go (ls@(l : ls'), gs@(((lnS, lnE), nonBlank -> multiMarkers) : gs'))
          | lineNumber < lnS = Just (MarkedLine{multiMarkers = [], nextMarkers = multiMarkers, ..}, (ls', gs))
          | lineNumber > lnE = go (ls, gs')
          | otherwise = Just (MarkedLine{..}, (ls', gs))
          where (isRealSource, lineNumber, nonBlank -> singleMarkers) = l
                lineText = fileLines A.! pred lineNumber
                nextMarkers = maybe [] (nonBlank . snd . fst) (uncons gs')

data MarkedLine msg = MarkedLine
  { isRealSource  :: !Bool
  , lineNumber    :: {-# UNPACK #-} !Line
  , lineText      :: String
  , singleMarkers :: [SingleMarker msg]
  , multiMarkers  :: [MultiMarker msg]
  , nextMarkers   :: [MultiMarker msg]
  }

mergeMarkers :: [(Line, [SingleMarker msg])] -> [Line] -> [(Line, [SingleMarker msg])]
mergeMarkers []       ys       = map (, []) ys
mergeMarkers xs       []       = xs
mergeMarkers (x : xs) (y : ys) = case compare (fst x) y of
  LT -> x : mergeMarkers xs (y : ys)
  EQ -> x : mergeMarkers xs ys
  GT -> (y, []) : mergeMarkers (x : xs) ys

fillGap :: [(Line, [SingleMarker msg])] -> [(Bool, Line, [SingleMarker msg])]
fillGap ((lnX, x) : xs@((lnY, _) : _))
  | lnX + 1 == lnY = (True, lnX, x) : fillGap xs
  | lnX + 2 == lnY = (True, lnX, x) : (True, succ lnX, []) : fillGap xs
  | otherwise = (True, lnX, x) : (False, succ lnX, []) : fillGap xs
fillGap xs = map (\(ln, t) -> (True, ln, t)) xs

{-
  Report structure
  ================

  1. One header (severity, error code, and message)
  2. One sub-report for each mentioned file
  3. Notes and helps attached to this report
-}

header :: Pretty msg => Severity -> Maybe msg -> msg -> Doc Annotation
header sev code msg
  -- header: 'error[E0001]'
  = annotate (Header sev) (pretty sev <> maybe emptyDoc (brackets . pretty) code)
  -- message: ': unexpected type in `+` application'
  <> annotate HeaderMessage (colon <+> align (pretty msg)) <> hardline

snippetStart :: Chars -> Int -> SourceRange -> Doc Annotation
snippetStart Chars{ cSnippetStart } k Range{ file, begin = (ln, col) }
  -- rendered as: '  ┌─ test:2:9'
  = pad k "" <+> annotate SourceBorder (pretty cSnippetStart)
  <+> pretty file <> colon <> pretty ln <> colon <> pretty col
  <> hardline

padWith :: Int -> String -> (Doc ann -> Doc ann) -> Doc ann
padWith w t f = pretty (replicate (w - length t) ' ') <> f (pretty t)

pad :: Int -> String -> Doc ann
pad w t = padWith w t id

-- note: we allow a one-pass-the-end index (to allow placing a caret here)
mkWidthTable :: Int -> String -> UArray Int Int
mkWidthTable tabSize s = A.array (1, length s + 1) $ zip [1..] $ scanl go 0 s
  where go n c = n + unicodeWidth tabSize n c

indexed :: [a] -> [(Int, a)]
indexed = zip [0..]

filterIndex :: (Int -> Bool) -> [a] -> [a]
filterIndex p = map snd . filter (p . fst) . indexed

filterIndexed :: (a -> Bool) -> [a] -> [(Int, a)]
filterIndexed p = filter (p . snd) . indexed

-- | 6. Render notes and helps (no markers: inline; with markers: sub-report)
renderInlineNote :: Pretty msg => Chars -> Int -> (Severity, msg) -> Doc Annotation
renderInlineNote Chars{ cNoteBullet } maxLnWidth (noteSev, noteMsg)
  -- '  = <kind>: <message>'
  = pad maxLnWidth "" <+> annotate NoteBullet (pretty cNoteBullet)
  <+> pretty noteSev <> colon <+> align (pretty noteMsg) <> hardline

newtype ExtInt = ExtInt Int deriving (Show, Eq)
instance Ord ExtInt where
  compare (ExtInt 0) (ExtInt _) = GT
  compare (ExtInt _) (ExtInt 0) = LT
  compare (ExtInt x) (ExtInt y) = compare x y

data ExtColumn = MaybeExtColumn
  { realColumn :: {-# UNPACK #-} !Int
  , extColumn  :: {-# UNPACK #-} !ExtInt
  } deriving (Show, Eq, Ord)

pattern RealColumn :: Int -> ExtColumn
pattern RealColumn n = MaybeExtColumn{ realColumn = n, extColumn = ExtInt 0 }

pattern ExtColumn :: Int -> Int -> ExtColumn
pattern ExtColumn l c = MaybeExtColumn{ realColumn = l, extColumn = ExtInt c }

nextColumn :: ExtColumn -> ExtColumn
nextColumn MaybeExtColumn{ realColumn } = MaybeExtColumn{ realColumn = realColumn + 1, extColumn = ExtInt 0 }

mergeAscendingOn :: Ord k => (a -> k) -> [a] -> [a] -> [a]
mergeAscendingOn key = go
  where go []       ys       = ys
        go xs       []       = xs
        go (x : xs) (y : ys) = case compare (key x) (key y) of
          LT -> x : go xs (y : ys)
          EQ -> error "mergeAscendingOn: EQ"
          GT -> y : go (x : xs) ys

extendLast :: (a -> a) -> [a] -> [a]
extendLast _ []          = error "extendLast: empty list"
extendLast f (x0 : rest) = go x0 rest
  where go x []       = [x, f x]
        go x (y : xs) = x : go y xs

-- | Rendered source line, with line number and multi-line markers on the left.
--
--   > 10 │   │ muffin. Halvah croissant candy canes bonbon candy. Apple pie jelly
--   >    │ ╭─│─────────^
sourceLine
  :: Pretty msg
  => Chars
  -> Int      -- ^ tab size.
  -> Int      -- ^ width for the line number.
  -> Int      -- ^ maximum number of multi-line markers.
  -> Severity -- ^ severity of the message for this line.
  -> MarkedLine msg
  -> Doc Annotation
sourceLine Chars{..} tabSize lnWidth maxMultiCount sev
  MarkedLine{ isRealSource, lineNumber = ln, lineText = (trimEnd -> text)
            , singleMarkers = singles, multiMarkers = multis, nextMarkers }
  -- > 10 │   │ muffin. Halvah croissant candy canes bonbon candy. Apple pie jelly
  = headLeader <+> attachColour decoratedText <> hardline
  -- >    │   │ ^^^^^^  -------^^^^^^^^^-------^^^^^------- ^^^^^ trailing label message
  <> (if null singles then emptyDoc else
        tailLeader <+> renderedMarkers <> trailingMsgRendered <> hardline)
  <> (if not anyDanglingMsg then emptyDoc else
  -- >    │   │ │              │
         allPointerLines <> hardline
  -- >    │   │ │              croissant is mentioned here
  -- >    │   │ muffin is first mentioned here
  -- >    │   │ help: the answer is 42
      <> drawDanglingMsgs (pred nDanglingMsgs))
  -- >    │ ╭─│─────────^
  <> foldMap renderMultiTopBottom (indexed multis)
  <> foldMap renderMultiTopBottom (indexed nextMarkers)
  where
    paddingForMultis = pad (2 * (maxMultiCount - length multis)) ""
    headLeader = lineNumber <+> leaders True
    tailLeader = pad lnWidth "" <+> leaders False
    lineNumber = if isRealSource then padWith lnWidth (show ln) (annotate LineNumber) else pad lnWidth ""
    -- handle leading multi-line markers
    leaders isSource = border <+> paddingForMultis <> hsep (map (leadingMarker isSource) multis)
    border = annotate SourceBorder (pretty if isRealSource then cSourceBorderLeft else cSourceBorderLeftBreak)
    leadingMarker isSource (((lnS, colS), (lnE, _)), markerStyle -> st)
      | lnS == ln, colS <= leadingSpaces, isSource = ann (pretty cMultiTopLeft)
      | lnS < ln, ln <= lnE = ann (pretty cMultiLeft)
      | otherwise = space
      where ann = annotate (MarkerTint sev st)
    leadingSpaces = length (takeWhile isSpace text)
    -- handle text insertion
    decoratedText
      = mergeAscendingOn fst insertions
      $ zip (map RealColumn [1..])
      $ zipWith handleTab [0..] text
    insertions = concat (mapMaybe go singles)
      where go ((l, _), m) = attachColumn l . map replaceNewline <$> markerInsertion m
            replaceNewline c = if generalCategory c == Control then ' ' else c
            attachColumn l = zip (map (ExtColumn l) [1..]) . zipWith handleTab [l - 1..]
    -- attach colour for the source code text
    attachColour
      = foldMap (renderSegment . (fst . head &&& concatMap snd))
      . groupBy ((==) `on` fst)
      . map (first styleOf)
    handleTab k '\t' = replicate (unicodeWidth tabSize k '\t') ' '
    handleTab _ c    = [c]
    renderSegment (st, s) = annotate (SourceTint sev st) (pretty s)
    maxStyle = minimum . (SBlank :) . map (markerStyle . snd)
    styleOf = uncurry min . (styleOfSingle &&& styleOfMulti)
    styleOfMulti col = maxStyle $ filter (inRange (ln, col) . mapRange (second RealColumn) . fst) multis
    styleOfSingle col = maxStyle sm
      where sm = filter (\(rng, markerStyle -> m) -> inRange col (liftRange m rng)) singles
            liftRange SAdd (l, r) = (ExtColumn l 1, ExtColumn l (r - l + 1))
            liftRange _    rng    = mapRange RealColumn rng
    -- handle single-line markers
    renderedMarkers
      = foldMap renderMarker
      $ dropWhileEnd ((== SBlank) . fst)
      $ map (fst . head &&& sum . map snd)
      $ groupBy ((==) `on` fst)
      $ map (first styleOfSingle)
      $ extendLast (bimap nextColumn (const 1))
      $ map (second (sum . map simpleWidth)) decoratedText
    renderMarker (st, k) = ann (pretty (replicate k c))
      where c = case st of
              SPrimary   -> cSinglePrimaryCaret
              SSecondary -> cSingleSecondaryCaret
              SAdd       -> cSingleAddCaret
              SRemove    -> cSingleRemoveCaret
              SBlank     -> ' '
            ann = if st == SBlank then id else annotate (MarkerTint sev st)
    trailingMsgRendered = maybe emptyDoc go trailingMsg
      where go (_, markerStyle &&& markerMessage -> (st, ~(Just msg)))
              = space <> align' st (pretty msg)
            align' st payload = column \cur -> replaceLinesWith
              (hardline <> fill cur allPointerLines)
              (annotate (MarkerTint sev st))
              payload
    trailingMsgCandidate = if null candidates then Nothing else Just res
      where col (_, ((_, colE), _)) = colE
            msg (_, (_, m)) = markerMessage m
            candidates = filter (isJust . msg) (zip @Int [0..] singles)
            res = maximumBy (comparing col) candidates
    trailingMsg
      -- found one last message
      | Just (idx, (rng, msg)) <- trailingMsgCandidate
      -- the source range does not overlap with any other
      , all (\(rng', _) -> not (isOverlapping rng rng'))
      $ filterIndex (/= idx) singles
      -- keep the index to avoid rendering it in dangling style again
      = Just (idx, msg)
      | otherwise = Nothing
    -- handle dangling messages for single-line markers
    widthTable = mkWidthTable tabSize text
    danglingMsgs
      = filter ((/= SBlank) . markerStyle . snd)
      $ filterIndex ((/= fmap fst trailingMsg) . Just) singles
    nDanglingMsgs = length danglingMsgs
    anyDanglingMsg = any (isJust . markerMessage . snd) danglingMsgs
    renderDanglingUntil k = foldl go emptyDoc (take k danglingMsgs)
      where go cur ((colS, _), m@(markerStyle -> st))
              | isNothing (markerMessage m) = emptyDoc
              | otherwise = fill (widthTable A.! colS) cur
                  <> annotate (MarkerTint sev st) (pretty cPointerLeft)
    allPointerLines = tailLeader <+> renderDanglingUntil nDanglingMsgs
    drawDanglingMsgs k
      | k < 0 = emptyDoc
      | withoutMessage = drawDanglingMsgs (pred k)
      | otherwise = leader <> pMsg <> hardline <> drawDanglingMsgs (pred k)
      where ((colS, _), marker) = danglingMsgs !! k
            (withoutMessage, msg) = maybe (True, undefined) (False, ) (markerMessage marker)
            st = markerStyle marker
            pMsg = replaceLinesWith (hardline <> leader) (annotate (MarkerTint sev st)) (pretty msg)
            leader = tailLeader <+> fill (widthTable A.! colS) (renderDanglingUntil k)
    -- handle multi-line markers (top & bottom rules)
    renderMultiTopBottom (k, (((lnS, colS), (lnE, colE)), outer))
      | lnE == ln = multiLeader True <> ann outerSt (replicate (pred colE) cMultiBottom ++ [ed]) <+> pMsg <> hardline
      | isStart = multiLeader True <> ann outerSt (replicate (pred colS) cMultiTop ++ [st]) <> hardline
      | otherwise = emptyDoc
      where leader = pad lnWidth "" <+> border <> paddingForMultis
            multiLeader isMain = leader <+> foldMap (multiMarkerLeft isMain) (indexed multis)
            isStart = lnS == ln && colS > leadingSpaces
            cBar = if isStart then cMultiTop else cMultiBottom
            pMsg = replaceLinesWith cont annDoc $ pretty $ fromJust $ markerMessage outer
            cont = hardline <> multiLeader False <> pretty (replicate (succ colE) ' ')
            outerSt = markerStyle outer
            annDoc = annotate (MarkerTint sev outerSt)
            ann :: Pretty a => MarkerStyle -> a -> Doc Annotation
            ann m = annotate (MarkerTint sev m) . pretty
            (st, ed) = case markerStyle outer of
              SPrimary   -> (cMultiPrimaryCaretStart, cMultiPrimaryCaretEnd)
              SSecondary -> (cMultiSecondaryCaretStart, cMultiSecondaryCaretEnd)
              SAdd       -> error "marker Add should not be multiline"
              SRemove    -> (cMultiRemoveCaretStart, cMultiRemoveCaretEnd)
              SBlank     -> error "impossible: unexpected Blank marker"
            multiMarkerLeft isMain (k', (((lnS', _), (lnE', _)), markerStyle -> inner))
              | through, k' < k            = ann inner cMultiLeft <> space
              | through                    = ann inner cMultiLeft <> pBar
              | lnS' == ln, k' < k         = ann inner cMultiLeft <> space
              | lnE' == ln, k' > k         = ann inner cMultiLeft <> pBar
              | lnS == ln, k' == k, isMain = ann outerSt [cMultiTopLeft, cMultiTop]
              | lnE == ln, k' == k, isMain = ann outerSt [cMultiBottomLeft, cMultiBottom]
              | k' > k, isMain             = ann outerSt [cBar, cBar]
              | otherwise                  = "  "
              where through = lnS' < ln && ln < lnE'
                    pBar = if isMain then ann outerSt cBar else space

trim, trimStart, trimEnd :: String -> String
trim = trimStart . trimEnd
trimStart = dropWhile isSpace
trimEnd = dropWhileEnd isSpace

-- WARN: uses the internal of the library 'prettyprinter'
--
--       DO NOT use a wildcard here, in case the internal API exposes one more constructor

-- |
replaceLinesWith :: Doc ann -> (Doc ann -> Doc ann) -> Doc ann -> Doc ann
replaceLinesWith repl t = go
  where
    go Line = repl
    go Fail = Fail
    go Empty = Empty
    go (Char '\n') = repl
    go (Char c) = Char c
    go (Text _ s)
      = mconcat
      $ intersperse repl
      $ map t
      $ uncurry Text . (T.length &&& id)
      <$> T.split (== '\n') s
    go (FlatAlt f d) = FlatAlt (go f) (go d)
    go (Cat c d) = Cat (go c) (go d)
    go (Nest n d) = Nest n (go d)
    go (Union c d) = Union (go c) (go d)
    go (Column f) = Column (go . f)
    go (Nesting f) = Nesting (go . f)
    go (Annotated ann doc) = Annotated ann (go doc)
    go (WithPageWidth f) = WithPageWidth (go . f)
