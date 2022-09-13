{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Error.Diagnose.Layout.CodeSpanReporting (codespanReportingLayout, defaultStyle) where

import qualified Data.Array.IArray as Array
import Data.Array.Unboxed (IArray, Ix, UArray, listArray, (!))
import Data.Bifunctor (bimap, first, second)
import qualified Data.Char.WCWidth as W (wcwidth)
import Data.Char (ord)
import Data.Default (def)
import Data.Foldable (fold)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.List as List
import qualified Data.List.Safe as List
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import qualified Data.Text as Text
import Error.Diagnose.Diagnostic (filesOf, reportsOf)
import Error.Diagnose.Layout (FileMap, Layout)
import Error.Diagnose.Position (Position (..))
import Error.Diagnose.Report (Marker (..), Note (..), Report (..))
import Error.Diagnose.Style (Annotation (..), Style, reAnnotate)
import Prettyprinter (Doc, Pretty, align, annotate, brackets, colon, emptyDoc, hardline, pretty, space, width, (<+>))
import Prettyprinter.Internal.Type (Doc (..))
import Text.Printf (printf)

import Error.Diagnose (bold, color, Color(..), colorDull)
import qualified Error.Diagnose.Layout.CodeSpanReporting.Config as R
import qualified Error.Diagnose.Layout.CodeSpanReporting.Render as R

-- | Pretty prints a 'Diagnostic' into a 'Doc'ument that can be output using 'hPutDoc'.
--
--   Colors are put by default.
--   If you do not want these, just 'unAnnotate' the resulting document like so:
--
--   >>> let doc = unAnnotate (prettyDiagnostic withUnicode tabSize diagnostic)
--
--   Changing the style is also rather easy:
--
--   >>> let myCustomStyle :: Style = _
--   >>> let doc = myCustomStyle (prettyDiagnostic withUnicode tabSize diagnostic)
codespanReportingLayout :: Layout R.Annotation msg
codespanReportingLayout withUnicode tabSize diag
  = foldMap (R.report (filesOf diag) chars tabSize) (reportsOf diag)
  -- fold . intersperse hardline $ prettyReport (filesOf diag) withUnicode tabSize <$> reportsOf diag
  where chars = if withUnicode then R.unicodeChars else R.asciiChars
{-# INLINE codespanReportingLayout #-}

defaultStyle :: Style R.Annotation
defaultStyle = reAnnotate \case
  R.Header R.Bug     -> bold <> color Red
  R.Header R.Error   -> bold <> color Red
  R.Header R.Warning -> bold <> color Yellow
  R.Header R.Note    -> bold <> color Green
  R.Header R.Help    -> bold <> color Cyan
  R.HeaderMessage -> bold <> color White
  R.SourceBorder -> colorDull Cyan -- Blue
  R.NoteBullet -> colorDull Cyan -- Blue
  R.LineNumber -> colorDull Cyan -- Blue
  R.SourceTint sev sty -> marker sev sty
  R.MarkerTint sev sty -> marker sev sty
  where marker R.Bug     R.SThis  = colorDull Red
        marker R.Error   R.SThis  = colorDull Red
        marker R.Warning R.SThis  = colorDull Yellow
        marker R.Note    R.SThis  = colorDull Green
        marker R.Help    R.SThis  = colorDull Cyan
        marker _         R.SBlank = mempty
        marker _         _        = colorDull Cyan -- Blue

--------------------------------------
------------- INTERNAL ---------------
--------------------------------------

type WidthTable = UArray Int Int

-- | Pretty prints a report to a 'Doc' handling colors.
prettyReport ::
  Pretty msg =>
  -- | The content of the file the reports are for
  FileMap ->
  -- | Should we print paths in unicode?
  Bool ->
  -- | The number of spaces each TAB character will span
  Int ->
  -- | The whole report to output
  Report msg ->
  Doc Annotation
prettyReport fileContent withUnicode tabSize (Warn code message markers hints) =
  prettyReport' fileContent withUnicode tabSize False code message markers hints
prettyReport fileContent withUnicode tabSize (Err code message markers hints) =
  prettyReport' fileContent withUnicode tabSize True code message markers hints

prettyReport' ::
  Pretty msg =>
  FileMap ->
  Bool ->
  Int ->
  Bool ->
  Maybe msg ->
  msg ->
  [(Position, Marker msg)] ->
  [Note msg] ->
  Doc Annotation
prettyReport' fileContent withUnicode tabSize isError code message markers hints =
  let sortedMarkers = List.sortOn (fst . begin . fst) markers
      -- sort the markers so that the first lines of the reports are the first lines of the file

      groupedMarkers = groupMarkersPerFile sortedMarkers
      -- group markers by the file they appear in, and put `This` markers at the top of the report

      maxLineNumberLength = maybe 3 (max 3 . length . show . fst . end . fst) $ List.safeLast markers
      -- if there are no markers, then default to 3, else get the maximum between 3 and the length of the last marker

      header =
        annotate
          (KindColor isError)
          ((if isError then "error" else "warning")
            <> maybe emptyDoc (brackets . pretty) code)
   in {-
              A report is of the form:
              (1)    <severity>[<code>]: <message>
              (2)           --> <file:line:column>
              (3)           |
              (4)    <line> | <line of code>
              (5)           | <marker & messages>
              (6)           |
              (7)           = <hints>
      -}

      {- (1) -} header <> colon <+> align (pretty message)
        <> {- (2)..(6) -} fold (uncurry (prettySubReport fileContent withUnicode isError tabSize maxLineNumberLength) <$> groupedMarkers)
        <> {- (7) -} prettyAllHints hints maxLineNumberLength withUnicode

-------------------------------------------------------------------------------------
----- INTERNAL STUFF ----------------------------------------------------------------
-------------------------------------------------------------------------------------

-- | Inserts a given number of character after a 'Doc'ument.
pad :: Int -> Char -> Doc ann -> Doc ann
pad n c d = width d \w -> pretty $ replicate (n - w) c

-- | Creates a "="-prefix for hints at the end of the report.
--
--   Pretty printing yields those results:
--
--   "@␣␣␣␣␣=␣@"
hintBullet ::
  -- | The length of the left space before the bullet.
  Int ->
  Doc Annotation
hintBullet leftLen = pad leftLen ' ' emptyDoc <+> annotate RuleColor "="
{-# INLINE hintBullet #-}

-- | Creates a "dot"-prefix for a report line where there is no code.
--
--   Pretty printing yields those results:
--
--   [with unicode] "@␣␣␣␣␣•␣@"
--   [without unicode] "@␣␣␣␣␣:␣@"
dotPrefix ::
  -- | The length of the left space before the bullet.
  Int ->
  -- | Whether to print with unicode characters or not.
  Bool ->
  Doc Annotation
dotPrefix leftLen withUnicode = pad leftLen ' ' emptyDoc <+> annotate RuleColor (if withUnicode then "•" else ":")
{-# INLINE dotPrefix #-}

-- | Creates a "pipe"-prefix for a report line where there is no code.
--
--   Pretty printing yields those results:
--
--   [with unicode] "@␣␣␣␣␣│␣@"
--   [without unicode] "@␣␣␣␣␣|␣@"
pipePrefix ::
  -- | The length of the left space before the pipe.
  Int ->
  -- | Whether to print with unicode characters or not.
  Bool ->
  Doc Annotation
pipePrefix leftLen withUnicode = pad leftLen ' ' emptyDoc <+> annotate RuleColor (if withUnicode then "│" else "|")
{-# INLINE pipePrefix #-}

-- | Creates a line-prefix for a report line containing source code
--
--   Pretty printing yields those results:
--
--   [with unicode] "@␣␣␣3␣│␣@"
--   [without unicode] "@␣␣␣3␣|␣@"
--
--   Results may be different, depending on the length of the line number.
linePrefix ::
  -- | The length of the amount of space to span before the vertical bar.
  Int ->
  -- | The line number to show.
  Int ->
  -- | Whether to use unicode characters or not.
  Bool ->
  Doc Annotation
linePrefix leftLen lineNo withUnicode =
  let lineNoLen = length (show lineNo)
   in annotate RuleColor $ emptyDoc <+> pad (leftLen - lineNoLen) ' ' emptyDoc <> pretty lineNo <+> if withUnicode then "│" else "|"
{-# INLINE linePrefix #-}

-- | Creates an ellipsis-prefix, when some line numbers are not consecutive.
--
--   Pretty printing yields those results:
--
--   [with unicode] "@␣␣␣␣␣⋮␣@"
--   [without unicode] "@␣␣␣␣...@"
ellipsisPrefix ::
  Int ->
  Bool ->
  Doc Annotation
ellipsisPrefix leftLen withUnicode = pad leftLen ' ' emptyDoc <> annotate RuleColor (if withUnicode then space <> "⋮" else "...")

groupMarkersPerFile ::
  Pretty msg =>
  [(Position, Marker msg)] ->
  [(Bool, [(Position, Marker msg)])]
groupMarkersPerFile [] = []
groupMarkersPerFile markers =
  let markersPerFile = List.foldl' (HashMap.unionWith (<>)) HashMap.empty $ markers <&> \tup@(p, _) -> HashMap.singleton (file p) [tup]
   in -- put all markers on the same file together
      -- NOTE: it's a shame that `HashMap.unionsWith f = foldl' (HashMap.unionWith f) emptyDoc` does not exist

      onlyFirstToTrue $ putThisMarkersAtTop $ HashMap.elems markersPerFile
  where
    onlyFirstToTrue = go True []

    go _ acc [] = reverse acc
    go t acc (x : xs) = go False ((t, x) : acc) xs

    putThisMarkersAtTop = List.sortBy \ms1 ms2 ->
      if
          | any isThisMarker (snd <$> ms1) -> LT
          | any isThisMarker (snd <$> ms2) -> GT
          | otherwise -> EQ

prettyPosition :: Position -> Doc Annotation
prettyPosition (Position (l, c) _ file) = pretty file <> colon <> pretty l <> colon <> pretty c

-- | Prettyprint a sub-report, which is a part of the report spanning across a single file
prettySubReport ::
  Pretty msg =>
  -- | The content of files in the diagnostics
  FileMap ->
  -- | Is the output done with Unicode characters?
  Bool ->
  -- | Is the current report an error report?
  Bool ->
  -- | The number of spaces each TAB character will span
  Int ->
  -- | The size of the biggest line number
  Int ->
  -- | Is this sub-report the first one in the list?
  Bool ->
  -- | The list of line-ordered markers appearing in a single file
  [(Position, Marker msg)] ->
  Doc Annotation
prettySubReport fileContent withUnicode isError tabSize maxLineNumberLength isFirst markers =
  let (markersPerLine, multilineMarkers) = splitMarkersPerLine markers
      -- split the list on whether markers are multiline or not

      sortedMarkersPerLine = {- second (List.sortOn (first $ snd . begin)) <$> -} List.sortOn fst (HashMap.toList markersPerLine)

      reportFile = prettyPosition $ maybe def fst $ List.safeHead (List.sortOn (Down . snd) markers)
      -- the reported file is the file of the first 'This' marker (only one must be present)

      allLineNumbers = List.sort $ List.nub $ (fst <$> sortedMarkersPerLine) <> (multilineMarkers >>= \(Position (bl, _) (el, _) _, _) -> [bl .. el])

      fileMarker =
        space <> pad maxLineNumberLength ' ' emptyDoc
          <+> annotate RuleColor (if withUnicode then "┌─" else "-->")
          <+> annotate FileColor reportFile
   in {- (2) -} hardline <> fileMarker <> hardline
        <+> {- (3) -} pipePrefix maxLineNumberLength withUnicode
        <> {- (4) -} prettyAllLines fileContent withUnicode isError tabSize maxLineNumberLength sortedMarkersPerLine multilineMarkers allLineNumbers

isThisMarker :: Marker msg -> Bool
isThisMarker (This _) = True
isThisMarker _ = False

-- |
splitMarkersPerLine :: [(Position, Marker msg)] -> (HashMap Int [(Position, Marker msg)], [(Position, Marker msg)])
splitMarkersPerLine [] = (HashMap.empty, [])
splitMarkersPerLine (m@(Position {..}, _) : ms) =
  let (bl, _) = begin
      (el, _) = end
   in (if bl == el then first (HashMap.insertWith (<>) bl [m]) else second (m :))
        (splitMarkersPerLine ms)

-- |
prettyAllLines ::
  Pretty msg =>
  FileMap ->
  Bool ->
  Bool ->
  -- | The number of spaces each TAB character will span
  Int ->
  Int ->
  [(Int, [(Position, Marker msg)])] ->
  [(Position, Marker msg)] ->
  [Int] ->
  Doc Annotation
prettyAllLines files withUnicode isError tabSize leftLen inline multiline lineNumbers =
  case lineNumbers of
    [] ->
      showMultiline True multiline
    [l] ->
      let (ms, doc) = showForLine True l
       in doc
            <> prettyAllLines files withUnicode isError tabSize leftLen inline ms []
    l1 : l2 : ls ->
      let (ms, doc) = showForLine False l1
       in doc
            <> (if l2 /= l1 + 1 then hardline <+> dotPrefix leftLen withUnicode else emptyDoc)
            <> prettyAllLines files withUnicode isError tabSize leftLen inline ms (l2 : ls)
  where
    showForLine isLastLine line =
      {-
          A line of code is composed of:
          (1)     <line> | <source code>
          (2)            | <marker & messages>

          Multline markers may further indent the code:
          (1)     <line> │ ╭   <source code>
          (2)     <line> │ │   
                  <line> │ │ ╭ 
                  <line> │ │ │ 
          (3)            · │ │
                  <line> │ │ │ 
          (4)            │ │ ╰' <message>
                  <line> │ │   else
          (5)            │ ╰───^ <message>
      -}
      let allInlineMarkersInLine = snd =<< filter ((==) line . fst) inline

          allMultilineMarkersInLine = flip filter multiline \(Position (bl, _) (el, _) _, _) -> bl == line || el == line

          allMultilineMarkersSpanningLine = flip filter multiline \(Position (bl, _) (el, _) _, _) -> bl < line && el > line

          inSpanOfMultiline = flip any multiline \(Position (bl, _) (el, _) _, _) -> bl <= line && el >= line

          colorOfFirstMultilineMarker = maybe id (annotate . markerColor isError . snd) (List.safeHead $ allMultilineMarkersInLine <> allMultilineMarkersSpanningLine)
          -- take the first multiline marker to color the entire line, if there is one

          (multilineEndingOnLine, otherMultilines) = flip List.partition multiline \(Position _ (el, _) _, _) -> el == line

          !additionalPrefix = case allMultilineMarkersInLine of
            [] ->
              if not $ null multiline
                then
                  if not $ null allMultilineMarkersSpanningLine
                    then colorOfFirstMultilineMarker if withUnicode then "│  " else "|  "
                    else "   "
                else emptyDoc
            (p@(Position _ (el, _) _), marker) : _ ->
              let hasPredecessor = el == line || maybe False ((/=) p . fst . fst) (List.safeUncons multiline)
               in colorOfFirstMultilineMarker
                    ( if
                          | hasPredecessor && withUnicode -> "├"
                          | hasPredecessor -> "|"
                          | withUnicode -> "╭"
                          | otherwise -> "+"
                    )
                    <> annotate (markerColor isError marker) (if withUnicode then "┤" else ">")
                    <> space

          -- we need to remove all blank markers because they are irrelevant to the display
          allInlineMarkersInLine' = filter ((/=) Blank . snd) allInlineMarkersInLine
          allMultilineMarkersSpanningLine' = filter ((/=) Blank . snd) allMultilineMarkersSpanningLine

          (widths, renderedCode) = getLine_ files (allInlineMarkersInLine <> allMultilineMarkersInLine <> allMultilineMarkersSpanningLine') line tabSize isError
       in ( otherMultilines,
            hardline
              <> {- (1) -} linePrefix leftLen line withUnicode <+> additionalPrefix
              <> renderedCode
              <> {- (2) -} showAllMarkersInLine (not $ null multiline) inSpanOfMultiline colorOfFirstMultilineMarker withUnicode isError leftLen widths allInlineMarkersInLine'
              <> showMultiline (isLastLine || List.safeLast multilineEndingOnLine == List.safeLast multiline) multilineEndingOnLine
          )

    showMultiline _ [] = emptyDoc
    showMultiline isLastMultiline multiline =
      let colorOfFirstMultilineMarker = markerColor isError . snd <$> List.safeHead multiline
          -- take the color of the last multiline marker in case we need to add additional bars

          prefix = hardline <+> dotPrefix leftLen withUnicode <> space

          prefixWithBar color = prefix <> maybe id annotate color (if withUnicode then "│ " else "| ")

          showMultilineMarkerMessage (_, Blank) _ = emptyDoc
          showMultilineMarkerMessage (_, marker) isLast =
            annotate (markerColor isError marker) $
              ( if isLast && isLastMultiline
                  then if withUnicode then "╰╸ " else "`- "
                  else if withUnicode then "├╸ " else "|- "
              )
                <> replaceLinesWith (if isLast then prefix <> "   " else prefixWithBar (Just $ markerColor isError marker) <> space) (pretty $ markerMessage marker)

          showMultilineMarkerMessages [] = []
          showMultilineMarkerMessages [m] = [showMultilineMarkerMessage m True]
          showMultilineMarkerMessages (m : ms) = showMultilineMarkerMessage m False : showMultilineMarkerMessages ms
       in prefixWithBar colorOfFirstMultilineMarker <> prefix <> fold (List.intersperse prefix $ showMultilineMarkerMessages multiline)

-- |
getLine_ ::
  FileMap ->
  [(Position, Marker msg)] ->
  Int ->
  Int ->
  Bool ->
  (WidthTable, Doc Annotation)
getLine_ files markers line tabSize isError =
  case safeArrayIndex (line - 1) =<< (HashMap.!?) files . file . fst =<< List.safeHead markers of
    Nothing ->
      ( mkWidthTable "",
        annotate NoLineColor "<no line>"
      )
    Just code ->
      ( mkWidthTable code,
        flip foldMap (zip [1 ..] code) \(n, c) ->
          let cdoc = ifTab (pretty (replicate tabSize ' ')) pretty c
              colorizingMarkers = flip filter markers \case
                (Position (bl, bc) (el, ec) _, _)
                  | bl == el ->
                    n >= bc && n < ec
                  | otherwise ->
                    (bl == line && n >= bc)
                      || (el == line && n < ec)
                      || (bl < line && el > line)
           in maybe
                (annotate CodeStyle)
                ((\m -> annotate (MarkerStyle $ markerColor isError m)) . snd)
                (List.safeHead colorizingMarkers)
                cdoc
      )
  where
    ifTab :: a -> (Char -> a) -> Char -> a
    ifTab a _ '\t' = a
    ifTab _ f c = f c

    mkWidthTable :: String -> WidthTable
    mkWidthTable s = listArray (1, length s) (ifTab tabSize wcwidth <$> s)

    wcwidth :: Char -> Int
    wcwidth c@(W.wcwidth -> w)
      | w >= 0    = w
      | otherwise = error (printf "negative width for '%c' (0x%04x)" c (ord c))

-- |
showAllMarkersInLine :: Pretty msg => Bool -> Bool -> (Doc Annotation -> Doc Annotation) -> Bool -> Bool -> Int -> WidthTable -> [(Position, Marker msg)] -> Doc Annotation
showAllMarkersInLine _ _ _ _ _ _ _ [] = emptyDoc
showAllMarkersInLine hasMultilines inSpanOfMultiline colorMultilinePrefix withUnicode isError leftLen widths ms =
  let maxMarkerColumn = snd $ end $ fst $ List.last $ List.sortOn (snd . end . fst) ms
      specialPrefix
        | inSpanOfMultiline = colorMultilinePrefix (if withUnicode then "│ " else "| ") <> space
        | hasMultilines = colorMultilinePrefix "  " <> space
        | otherwise = emptyDoc
   in -- get the maximum end column, so that we know when to stop looking for other markers on the same line
      hardline <+> dotPrefix leftLen withUnicode <+> (if List.null ms then emptyDoc else specialPrefix <> showMarkers 1 maxMarkerColumn <> showMessages specialPrefix ms maxMarkerColumn)
  where
    widthAt i = 0 `fromMaybe` safeArrayIndex i widths
    widthsBetween start end =
      sum $ take (end - start) $ drop (start - 1) $ Array.elems widths

    showMarkers n lineLen
      | n > lineLen = emptyDoc -- reached the end of the line
      | otherwise =
        let allMarkers = flip filter ms \(Position (_, bc) (_, ec) _, mark) -> mark /= Blank && n >= bc && n < ec
         in -- only consider markers which span onto the current column
            case allMarkers of
              [] -> fold (replicate (widthAt n) space) <> showMarkers (n + 1) lineLen
              (Position {..}, marker) : _ ->
                annotate
                  (markerColor isError marker)
                  ( if snd begin == n
                      then (if withUnicode then "┬" else "^") <> fold (replicate (widthAt n - 1) if withUnicode then "─" else "-")
                      else fold (replicate (widthAt n) if withUnicode then "─" else "-")
                  )
                  <> showMarkers (n + 1) lineLen

    showMessages specialPrefix ms lineLen = case List.safeUncons ms of
      Nothing -> emptyDoc -- no more messages to show
      Just ((Position b@(_, bc) _ _, msg), pipes) ->
        let filteredPipes = filter (uncurry (&&) . bimap ((/= b) . begin) (/= Blank)) pipes
            -- record only the pipes corresponding to markers on different starting positions
            nubbedPipes = List.nubBy ((==) `on` (begin . fst)) filteredPipes
            -- and then remove all duplicates

            allColumns _ [] = (1, [])
            allColumns n ms@((Position (_, bc) _ _, col) : ms')
              | n == bc = bimap (+ 1) (col :) (allColumns (n + 1) ms')
              | n < bc = bimap (+ 1) (replicate (widthAt n) space <>) (allColumns (n + 1) ms)
              | otherwise = bimap (+ 1) (replicate (widthAt n) space <>) (allColumns (n + 1) ms')
            -- transform the list of remaining markers into a single document line

            hasSuccessor = length filteredPipes /= length pipes

            lineStart pipes =
              let (n, docs) = allColumns 1 $ List.sortOn (snd . begin . fst) pipes
                  numberOfSpaces = widthsBetween n bc
               in dotPrefix leftLen withUnicode <+> specialPrefix <> fold docs <> pretty (replicate numberOfSpaces ' ')
            -- the start of the line contains the "dot"-prefix as well as all the pipes for all the still not rendered marker messages

            prefix =
              let (pipesBefore, pipesAfter) = List.partition ((< bc) . snd . begin . fst) nubbedPipes
                  -- split the list so that all pipes before can have `|`s but pipes after won't

                  pipesBeforeRendered = pipesBefore <&> second \marker -> annotate (markerColor isError marker) (if withUnicode then "│" else "|")
                  -- pre-render pipes which are before because they will be shown

                  lastBeginPosition = snd . begin . fst <$> List.safeLast (List.sortOn (snd . begin . fst) pipesAfter)

                  lineLen = case lastBeginPosition of
                    Nothing -> 0
                    Just col -> widthsBetween bc col

                  currentPipe =
                    if
                        | withUnicode && hasSuccessor -> "├"
                        | withUnicode -> "╰"
                        | hasSuccessor -> "|"
                        | otherwise -> "`"

                  lineChar = if withUnicode then '─' else '-'
                  pointChar = if withUnicode then "╸" else "-"

                  bc' = bc + lineLen + 2
                  pipesBeforeMessageStart = List.filter ((< bc') . snd . begin . fst) pipesAfter
                  -- consider pipes before, as well as pipes which came before the text rectangle bounds
                  pipesBeforeMessageRendered = (pipesBefore <> pipesBeforeMessageStart) <&> second \marker -> annotate (markerColor isError marker) (if withUnicode then "│" else "|")
               in -- also pre-render pipes which are before the message text bounds, because they will be shown if the message is on
                  -- multiple lines

                  lineStart pipesBeforeRendered
                    <> annotate (markerColor isError msg) (currentPipe <> pretty (replicate lineLen lineChar) <> pointChar)
                    <+> annotate (markerColor isError msg) (replaceLinesWith (hardline <+> lineStart pipesBeforeMessageRendered <+> if List.null pipesBeforeMessageStart then "  " else " ") $ pretty $ markerMessage msg)
         in hardline <+> prefix <> showMessages specialPrefix pipes lineLen

-- WARN: uses the internal of the library 'prettyprinter'
--
--       DO NOT use a wildcard here, in case the internal API exposes one more constructor

-- |
replaceLinesWith :: Doc ann -> Doc ann -> Doc ann
replaceLinesWith repl Line = repl
replaceLinesWith _    Fail = Fail
replaceLinesWith _    Empty = Empty
replaceLinesWith repl (Char '\n') = repl
replaceLinesWith _    (Char c) = Char c
replaceLinesWith repl (Text _ s) =
  let lines = Text.split (== '\n') s <&> \txt -> Text (Text.length txt) txt
   in mconcat (List.intersperse repl lines)
replaceLinesWith repl (FlatAlt f d) = FlatAlt (replaceLinesWith repl f) (replaceLinesWith repl d)
replaceLinesWith repl (Cat c d) = Cat (replaceLinesWith repl c) (replaceLinesWith repl d)
replaceLinesWith repl (Nest n d) = Nest n (replaceLinesWith repl d)
replaceLinesWith repl (Union c d) = Union (replaceLinesWith repl c) (replaceLinesWith repl d)
replaceLinesWith repl (Column f) = Column (replaceLinesWith repl . f)
replaceLinesWith repl (Nesting f) = Nesting (replaceLinesWith repl . f)
replaceLinesWith repl (Annotated ann doc) = Annotated ann (replaceLinesWith repl doc)
replaceLinesWith repl (WithPageWidth f) = WithPageWidth (replaceLinesWith repl . f)

-- | Extracts the color of a marker as a 'Doc' coloring function.
markerColor ::
  -- | Whether the marker is in an error context or not.
  --   This really makes a difference for a 'This' marker.
  Bool ->
  -- | The marker to extract the color from.
  Marker msg ->
  -- | A function used to color a 'Doc'.
  Annotation
markerColor isError (This _) = ThisColor isError
markerColor _ (Where _) = WhereColor
markerColor _ (Maybe _) = MaybeColor
markerColor _ Blank = CodeStyle -- we take the same color as the code, for it to be invisible
{-# INLINE markerColor #-}

-- | Retrieves the message held by a marker.
markerMessage :: Marker msg -> msg
markerMessage (This m) = m
markerMessage (Where m) = m
markerMessage (Maybe m) = m
markerMessage Blank = undefined
{-# INLINE markerMessage #-}

-- | Pretty prints all hints.
prettyAllHints :: Pretty msg => [Note msg] -> Int -> Bool -> Doc Annotation
prettyAllHints [] _ _ = emptyDoc
prettyAllHints (h : hs) leftLen withUnicode =
  {-
        A hint is composed of:
        (1)         = hint: <hint message>
  -}
  let prefix = hardline <+> hintBullet leftLen
   in prefix <+> annotate HintColor (notePrefix h <+> pretty (noteMessage h))
        <> prettyAllHints hs leftLen withUnicode
  where
    notePrefix (Note _) = "note:"
    notePrefix (Hint _) = "hint:"

    noteMessage (Note msg) = msg
    noteMessage (Hint msg) = msg

safeArrayIndex :: (Ix i, IArray a e) => i -> a i e -> Maybe e
safeArrayIndex i a
  | Array.inRange (Array.bounds a) i = Just (a ! i)
  | otherwise = Nothing
