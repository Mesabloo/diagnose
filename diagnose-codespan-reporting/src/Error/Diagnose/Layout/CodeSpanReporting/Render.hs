{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE ViewPatterns     #-}
{-# LANGUAGE DerivingVia      #-}
module Error.Diagnose.Layout.CodeSpanReporting.Render where

import Error.Diagnose.Layout.CodeSpanReporting.Config

import qualified Data.Array.IArray as A (array, bounds, (!))
import qualified Data.HashMap.Lazy as H ((!?))
import qualified Data.Text as T (length, split)
import Data.Array.Unboxed (UArray)
import Data.Char (isSpace, ord)
import Data.Char.WCWidth (wcwidth)
import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.List (groupBy, nub, sort, sortOn, dropWhileEnd, intersperse)
import Data.Maybe (isJust, fromJust)
import Data.Ord (comparing)
import Control.Arrow ((&&&))
import Text.Printf (printf)

import Error.Diagnose (Position(..), Marker(..), Report(..), Note, align, hsep)
import qualified Error.Diagnose as E (Note(Note, Hint))
import Error.Diagnose.Layout (FileMap)
import Prettyprinter (Doc, Pretty(..), (<+>), annotate, brackets, emptyDoc, colon, space, hardline, column, fill)
import Prettyprinter.Internal (Doc(..))
import Debug.Trace (traceShowId)

unicodeWidth :: Int -> Int -> Char -> Int
unicodeWidth tabSize col c@(wcwidth -> w)
  | w >= 0    = w
  | c == '\t' = (col `div` tabSize + 1) * tabSize - col
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

data MarkerStyle
  = SThis
  | SWhere
  | SMaybe
  | SBlank
  deriving (Show, Eq, Ord)

nonBlank :: [(a, Marker msg)] -> [(a, Marker msg)]
nonBlank = filter ((/= SBlank) . markerStyle . snd)

markerStyle :: Marker msg -> MarkerStyle
markerStyle (This  _) = SThis
markerStyle (Where _) = SWhere
markerStyle (Maybe _) = SMaybe
markerStyle Blank     = SBlank

markerMessage :: Marker msg -> Maybe msg
markerMessage (This  msg) = Just msg
markerMessage (Where msg) = Just msg
markerMessage (Maybe msg) = Just msg
markerMessage Blank       = Nothing

reportComponents :: Report msg -> (Severity, Maybe msg, msg, [(Position, Marker msg)], [Note msg])
reportComponents (Warn code msg markers notes) = (Warning, code, msg, markers, notes)
reportComponents (Err  code msg markers notes) = (Error,   code, msg, markers, notes)

takeNAndOthers :: Pretty a => Int -> [a] -> Doc ann
takeNAndOthers 0 _              = error "takeNAndOthers: cannot take 0"
takeNAndOthers _ []             = error "takeNAndOthers: empty list"
takeNAndOthers n (first : rest) = pretty first <> go (pred n) rest
  where go _ []       = emptyDoc
        go 0 [x]      = ", and " <> pretty x
        go 0 others   = ", and " <> pretty (length others) <> " other(s)"
        go k (x : xs) = ", " <> pretty x <> go (pred k) xs

report :: Pretty msg => FileMap -> Chars -> Int -> Report msg -> Doc Annotation
report fileMap chars@Chars{ cNoteBullet, cSourceBorderLeft }
  tabSize (reportComponents -> (sev, code, msg, markers, notes))
  = header sev code msg <> foldMap renderFile groups
  <> foldMap renderNote notes
  where groups = sortMarkers markers
        maxLnWidth = length $ show $ maximum $ 0 : concatMap go markers
          where go (Position{ begin, end }, _) = [fst begin, snd end]
        leftPadding = pad maxLnWidth ""
        trailingLeftBorder = leftPadding <+> annotate SourceBorder (pretty cSourceBorderLeft) <> hardline
        sortMarkers
          = map (file . fst . head &&& id)
          . groupBy ((==) `on` file . fst)
          . sortOn (posToTriple . fst)
        posToTriple Position{ begin, end, file } = (file, begin, end)
        renderFile (fileName, thisMarkers@(classifyMarkers -> (groupSingles -> singleWithLines, multis)))
          | Just fileLines <- fileMap H.!? fileName
          , missingLines <- filter (not . (`inRange` A.bounds fileLines) . pred) lineNumbers
          = if null missingLines then
            let go (b, ln, singles) = sourceLine chars tabSize b ln maxLnWidth (fileLines A.! pred ln) sev singles multis
            in snippetStart chars maxLnWidth startPos <> foldMap go allLines <> trailingLeftBorder
          else makeBug ("line " <> takeNAndOthers 2 missingLines <> " of file '" <> pretty fileName <> "' not available")
          | otherwise = makeBug ("content of file '" <> pretty fileName <> "' not available")
          where lineNumbers = map fst singleWithLines ++ concatMap linesForMulti multis
                allLines = fillGap $ merge singleWithLines $ nub $ sort $ concatMap linesForMulti multis
                merge []       ys       = map (, []) ys
                merge xs       []       = xs
                merge (x : xs) (y : ys) = case compare (fst x) y of
                  LT -> x : merge xs (y : ys)
                  EQ -> x : merge xs ys
                  GT -> (y, []) : merge (x : xs) ys
                fillGap ((lnX, x) : xs@((lnY, _) : _))
                  | lnX + 1 == lnY = (True, lnX, x) : fillGap xs
                  | lnX + 2 == lnY = (True, lnX, x) : (True, succ lnX, []) : fillGap xs
                  | otherwise = (True, lnX, x) : (False, succ lnX, []) : fillGap xs
                fillGap xs = map (\(ln, t) -> (True, ln, t)) xs
                linesForMulti (((lnS, _), (lnE, _)), _) = [lnS, lnE]
                startPos = fst (head thisMarkers)
                makeBug s = leftPadding <+> annotate (Header Bug) "bug" <> annotate HeaderMessage (colon <+> s) <> hardline
        groupSingles = map (fst . head &&& map snd) . groupBy ((==) `on` fst)
        renderNote nt
          -- '  = <kind>: <message>'
          = leftPadding <+> annotate NoteBullet (pretty cNoteBullet)
          <+> pretty @String noteLevel <> colon <+> align (pretty noteMsg) <> hardline
          where (noteLevel, noteMsg) = case nt of
                  E.Hint m -> ("hint", m)
                  E.Note m -> ("note", m)

partitionEither :: (a -> Either b c) -> [a] -> ([b], [c])
partitionEither p = foldr go ([], [])
  where go (p -> Left b)  ~(bs, cs) = (b : bs, cs)
        go (p -> Right c) ~(bs, cs) = (bs, c : cs)

classifyMarkers :: [(Position, Marker msg)] -> ([(Line, SingleMarker msg)], [MultiMarker msg])
classifyMarkers = partitionEither \(pos, marker) ->
  let Position{ begin = begin@(lnS, colS), end = end@(lnE, colE) } = pos
  in if lnS == lnE then Left (lnS, ((colS, colE), marker)) else Right ((begin, end), marker)

header :: Pretty msg => Severity -> Maybe msg -> msg -> Doc Annotation
header sev code msg
  -- header: 'error[E0001]'
  = annotate (Header sev) (pretty sev <> maybe emptyDoc (brackets . pretty) code)
  -- message: ': unexpected type in `+` application'
  <> annotate HeaderMessage (colon <+> align (pretty msg)) <> hardline

snippetStart :: Chars -> Int -> Position -> Doc Annotation
snippetStart Chars{ cSnippetStart } k Position{ file, begin = (ln, col) }
  -- rendered as: '  ┌─ test:2:9'
  = pad k "" <+> annotate SourceBorder (pretty cSnippetStart)
  <+> pretty file <> colon <> pretty ln <> colon <> pretty col
  <> hardline

padWith :: Int -> String -> (Doc ann -> Doc ann) -> Doc ann
padWith w t f = pretty (replicate (w - length t) ' ') <> f (pretty t)

pad :: Int -> String -> Doc ann
pad w t = padWith w t id

type Line = Int
type Column = Int
type Range a = (a, a)

inRange :: Ord a => a -> Range a -> Bool
x `inRange` (l, r) = l <= x && x <= r

isOverlapping :: Ord a => Range a -> Range a -> Bool
isOverlapping (l1, r1) (l2, r2) = r1 >= l2 && r2 >= l1

type SingleMarker msg = (Range Column, Marker msg)
type MultiMarker msg = (Range (Line, Column), Marker msg)

-- note: we allow a one-pass-the-end index (to allow place a caret here)
mkWidthTable :: Int -> String -> UArray Int Int
mkWidthTable tabSize s = A.array (1, length s + 1) $ zip [1..] $ scanl go 0 s
  where go n c = n + unicodeWidth tabSize n c

indexed :: [a] -> [(Int, a)]
indexed = zip [0..]

filterIndex :: (Int -> Bool) -> [a] -> [a]
filterIndex p = map snd . filter (p . fst) . indexed

filterIndexed :: (a -> Bool) -> [a] -> [(Int, a)]
filterIndexed p = filter (p . snd) . indexed

-- | Rendered source line, with line number and multi-line markers on the left.
--
--   > 10 │   │ muffin. Halvah croissant candy canes bonbon candy. Apple pie jelly
--   >    │ ╭─│─────────^
sourceLine
  :: Pretty msg
  => Chars
  -> Int      -- ^ tab size.
  -> Bool     -- ^ 'True' - real source line; 'False' - gap.
  -> Int      -- ^ line number.
  -> Int      -- ^ width for the line number.
  -> String   -- ^ source code.
  -> Severity -- ^ severity of the message for this line.
  -> [SingleMarker msg] -- single-line markers.
  -> [MultiMarker msg]  -- multi-line markers.
  -> Doc Annotation
sourceLine Chars{..} tabSize isRealSource ln lnWidth
  (trimEnd -> text) sev (nonBlank -> singles) (nonBlank -> multis)
  -- > 10 │   │ muffin. Halvah croissant candy canes bonbon candy. Apple pie jelly
  = headLeader <+> attachColour text <> hardline
  -- >    │   │ ^^^^^^  -------^^^^^^^^^-------^^^^^------- ^^^^^ trailing label message
  <> (if null singles then emptyDoc else
        tailLeader <+> drawMarkers text <> trailingMsgRendered <> hardline)
  <> (if nDanglingMsgs == 0 then emptyDoc else
  -- >    │   │ │              │
         allPointerLines <> hardline
  -- >    │   │ │              croissant is mentioned here
  -- >    │   │ muffin is first mentioned here
  -- >    │   │ help: the answer is 42
      <> drawDanglingMsgs (pred nDanglingMsgs))
  -- >    │ ╭─│─────────^
  <> foldMap renderMultiTopBottom (indexed multis)
  where
    headLeader = lineNumber <+> leaders True
    tailLeader = pad lnWidth "" <+> leaders False
    lineNumber = if isRealSource then padWith lnWidth (show ln) (annotate LineNumber) else pad lnWidth ""
    -- handle leading multi-line markers
    leaders isSource = border <+> hsep (map (leadingMarker isSource) multis)
    border = annotate SourceBorder (pretty if isRealSource then cSourceBorderLeft else cSourceBorderLeftBreak)
    leadingMarker isSource (((lnS, colS), (lnE, _)), markerStyle -> st)
      | lnS == ln, colS <= leadingSpaces, isSource = ann (pretty cMultiTopLeft)
      | lnS < ln, ln <= lnE = ann (pretty cMultiLeft)
      | otherwise = space
      where ann = annotate (MarkerTint sev st)
    leadingSpaces = length (takeWhile isSpace text)
    -- attach colour for the source code text
    attachColour
      = foldMap (renderSegment . (fst . head &&& concatMap snd))
      . groupBy ((==) `on` fst)
      . zip (map styleOf [1..])
      . zipWith handleTab [0..]
    handleTab k '\t' = replicate (unicodeWidth tabSize k '\t') ' '
    handleTab _ c    = [c]
    renderSegment (st, s) = annotate (SourceTint sev st) (pretty s)
    maxStyle = minimum . (SBlank :) . map (markerStyle . snd)
    styleOf col =
      let s = filter (inRange col . fst) singles
          m = filter (inRange (ln, col) . fst) multis
      in maxStyle s `min` maxStyle m
    -- handle single-line markers
    drawMarkers
      = foldMap renderMarker
      . dropWhileEnd ((== SBlank) . fst)
      . map (fst . head &&& sum . map snd)
      . groupBy ((==) `on` fst)
      . zip (map styleOfSingle [1..])
      . (++ [1])
      . zipWith (unicodeWidth tabSize) [0..]
    renderMarker (st, k) = ann (pretty (replicate k c))
      where c | SThis <- st = cSinglePrimaryCaret
              | SBlank <- st = ' '
              | otherwise = cSingleSecondaryCaret
            ann = if st == SBlank then id else annotate (MarkerTint sev st)
    styleOfSingle col = maxStyle (filter (inRange col . fst) singles)
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
    renderDanglingUntil k = foldl go emptyDoc (take k danglingMsgs)
      where go cur ((colS, _), markerStyle -> st)
              = fill (widthTable A.! colS) cur <> annotate (MarkerTint sev st) (pretty cPointerLeft)
    allPointerLines = tailLeader <+> renderDanglingUntil nDanglingMsgs
    drawDanglingMsgs k
      | k < 0 = emptyDoc
      | otherwise = leader <> pMsg <> hardline <> drawDanglingMsgs (pred k)
      where ((colS, _), marker) = danglingMsgs !! k
            ~(Just msg) = markerMessage marker
            st = markerStyle marker
            pMsg = replaceLinesWith (hardline <> leader) (annotate (MarkerTint sev st)) (pretty msg)
            leader = tailLeader <+> fill (widthTable A.! colS) (renderDanglingUntil k)
    -- handle multi-line markers (top & bottom rules)
    renderMultiTopBottom (k, (((lnS, colS), (lnE, colE)), outer))
      | lnE == ln = multiLeader True <> ann outerSt (replicate (pred colE) cMultiBottom ++ [ed]) <+> pMsg <> hardline
      | isStart = multiLeader True <> ann outerSt (replicate (pred colS) cMultiTop ++ [st]) <> hardline
      | otherwise = emptyDoc
      where leader = pad lnWidth "" <+> border
            multiLeader isMain = leader <+> foldMap (multiMarkerLeft isMain) (indexed multis)
            isStart = lnS == ln && colS > leadingSpaces
            cBar = if isStart then cMultiTop else cMultiBottom
            pMsg = replaceLinesWith cont annDoc $ pretty $ fromJust $ markerMessage outer
            cont = hardline <> multiLeader False <> pretty (replicate (succ colE) ' ')
            outerSt = markerStyle outer
            annDoc = annotate (MarkerTint sev outerSt)
            ann m = annotate (MarkerTint sev m) . pretty
            st = if markerStyle outer == SThis then cMultiPrimaryCaretStart else cMultiSecondaryCaretStart
            ed = if markerStyle outer == SThis then cMultiPrimaryCaretEnd else cMultiSecondaryCaretEnd
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
