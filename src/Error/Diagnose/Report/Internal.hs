{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS -Wno-name-shadowing #-}

-- |
-- Module      : Error.Diagnose.Report.Internal
-- Description : Internal workings for report definitions and pretty printing.
-- Copyright   : (c) Mesabloo, 2021-2022
-- License     : BSD3
-- Stability   : experimental
-- Portability : Portable
--
-- /Warning/: The API of this module can break between two releases, therefore you should not rely on it.
--            It is also highly undocumented.
--
--            Please limit yourself to the "Error.Diagnose.Report" module, which exports some of the useful functions defined here.
module Error.Diagnose.Report.Internal
  ( module Error.Diagnose.Report.Internal
  , Report(.., Warn, Err)
  , WithUnicode(..)
  , TabSize(..)
  ) where

#ifdef USE_AESON
import Data.Aeson (ToJSON(..), object, (.=))
#endif
import Control.Applicative ((<|>))
import qualified Data.Array.IArray as Array
import Data.Array.Unboxed (Array, IArray, Ix, UArray, listArray, (!))
import Data.Bifunctor (bimap, first, second)
import Data.Char.WCWidth (wcwidth)
import Data.Default (def)
import Data.Foldable (fold)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.List as List
import qualified Data.List.Safe as List
import Data.Maybe
import Data.Ord (Down (Down))
import Data.String (IsString (fromString))
import qualified Data.Text as Text
import Error.Diagnose.Position
import Error.Diagnose.Style (Annotation (..))
import Prettyprinter (Doc, Pretty (..), align, annotate, colon, hardline, lbracket, rbracket, space, width, (<+>), reAnnotate, SimpleDocStream (..), layoutCompact)
import Prettyprinter.Internal (Doc (..), textSpaces)
import Data.Bool (bool)

type FileMap = HashMap FilePath (Array Int String)

type WidthTable = UArray Int Int

-- | The type of diagnostic reports with abstract message type.
data Report msg
  = Report
      Bool
      -- ^ Is the report a warning or an error?
      (Maybe msg)
      -- ^ An optional error code to print at the top.
      msg
      -- ^ The message associated with the error.
      [(Position, Marker msg)]
      -- ^ A map associating positions with marker to show under the source code.
      [Note msg]
      -- ^ A list of notes to add at the end of the report.
  deriving (Functor, Foldable, Traversable)

-- | Pattern synonym for a warning report.
pattern Warn :: Maybe msg -> msg -> [(Position, Marker msg)] -> [Note msg] -> Report msg
pattern Warn errCode msg reports notes = Report False errCode msg reports notes

-- | Pattern synonym for an error report.
pattern Err :: Maybe msg -> msg -> [(Position, Marker msg)] -> [Note msg] -> Report msg
pattern Err errCode msg reports notes = Report True errCode msg reports notes

{-# COMPLETE Warn, Err #-}

instance Semigroup msg => Semigroup (Report msg) where
  Report isError1 code1 msg1 pos1 hints1 <> Report isError2 code2 msg2 pos2 hints2 =
    Report (isError1 || isError2) (code1 <|> code2) (msg1 <> msg2) (pos1 <> pos2) (hints1 <> hints2)

instance Monoid msg => Monoid (Report msg) where
  mempty = Report False Nothing mempty mempty mempty

#ifdef USE_AESON
instance ToJSON msg => ToJSON (Report msg) where
  toJSON (Report isError code msg markers hints) =
    object [ "kind" .= (if isError then "error" else "warning" :: String)
           , "code" .= code
           , "message" .= msg
           , "markers" .= fmap showMarker markers
           , "hints" .= hints
           ]
    where
      showMarker (pos, marker) =
        object $ [ "position" .= pos ]
              <> case marker of
                   This m  -> [ "message" .= m
                              , "kind" .= ("this" :: String)
                              ]
                   Where m -> [ "message" .= m
                              , "kind" .= ("where" :: String)
                              ]
                   Maybe m -> [ "message" .= m
                              , "kind" .= ("maybe" :: String)
                              ]
                   Blank -> [ "kind" .= ("blank" :: String) ]
#endif

-- | The type of markers with abstract message type, shown under code lines.
data Marker msg
  = -- | A red or yellow marker under source code, marking important parts of the code.
    This msg
  | -- | A blue marker symbolizing additional information.
    Where msg
  | -- | A magenta marker to report potential fixes.
    Maybe msg
  | -- | An empty marker, whose sole purpose is to include a line of code in the report without markers under.
    Blank
  deriving (Functor)

instance Eq (Marker msg) where
  This _ == This _ = True
  Where _ == Where _ = True
  Maybe _ == Maybe _ = True
  Blank == Blank = True
  _ == _ = False
  {-# INLINEABLE (==) #-}

instance Ord (Marker msg) where
  This _ < _ = False
  Where _ < This _ = True
  Where _ < _ = False
  Maybe _ < _ = True
  _ < Blank = True
  Blank < _ = False
  {-# INLINEABLE (<) #-}

  m1 <= m2 = m1 < m2 || m1 == m2
  {-# INLINEABLE (<=) #-}

-- | A note is a piece of information that is found at the end of a report.
data Note msg
  = -- | A note, which is meant to give valuable information related to the encountered error.
    Note msg
  | -- | A hint, to propose potential fixes or help towards fixing the issue.
    Hint msg
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

#ifdef USE_AESON
instance ToJSON msg => ToJSON (Note msg) where
  toJSON (Note msg) = object [ "note" .= msg ]
  toJSON (Hint msg) = object [ "hint" .= msg ]
#endif

-- | Constructs a 'Note' from the given message as a literal string.
instance IsString msg => IsString (Note msg) where
  fromString = Note . fromString

-- | Constructs a warning or an error report.
warn,
  err ::
    -- | An optional error code to be shown right next to "error" or "warning".
    Maybe msg ->
    -- | The report message, shown at the very top.
    msg ->
    -- | A list associating positions with markers.
    [(Position, Marker msg)] ->
    -- | A possibly mempty list of hints to add at the end of the report.
    [Note msg] ->
    Report msg
warn = Report False
{-# INLINE warn #-}
{-# DEPRECATED warn "'warn' is deprecated. Use 'Warn' instead." #-}
err = Report True
{-# INLINE err #-}
{-# DEPRECATED err "'err' is deprecated. Use 'Err' instead." #-}

-- | Transforms a warning report into an error report.
warningToError :: Report msg -> Report msg
warningToError (Report False code msg markers notes) = Report True code msg markers notes
warningToError r@(Report True _ _ _ _) = r

-- | Transforms an error report into a warning report.
errorToWarning :: Report msg -> Report msg
errorToWarning (Report True code msg markers notes) = Report False code msg markers notes
errorToWarning r@(Report False _ _ _ _) = r

data WithUnicode = WithoutUnicode | WithUnicode

newtype TabSize = TabSize Int

-- | Pretty prints a report to a 'Doc' handling colors.
prettyReport ::
  -- | The content of the file the reports are for
  FileMap ->
  -- | Should we print paths in unicode?
  WithUnicode ->
  -- | The number of spaces each TAB character will span
  TabSize ->
  -- | The whole report to output
  Report (Doc ann) ->
  Doc (Annotation ann)
prettyReport fileContent withUnicode tabSize (Report isError code message markers hints) =
  let sortedMarkers = List.sortOn (fst . begin . fst) markers
      -- sort the markers so that the first lines of the reports are the first lines of the file

      groupedMarkers = groupMarkersPerFile sortedMarkers
      -- group markers by the file they appear in, and put `This` markers at the top of the report

      maxLineNumberLength = maybe 3 (max 3 . length . show . fst . end . fst) $ List.safeLast markers
      -- if there are no markers, then default to 3, else get the maximum between 3 and the length of the last marker

      header =
        annotate
          (KindColor isError)
          ( lbracket
              <> ( if isError
                     then "error"
                     else "warning"
                 )
              <> case code of
                Nothing -> rbracket
                Just code -> space <> annotated code <> rbracket
          )
   in {-
              A report is of the form:
              (1)    [error|warning]: <message>
              (2)           +--> <file>
              (3)           :
              (4)    <line> | <line of code>
                            : <marker lines>
                            : <marker messages>
              (5)           :
                            : <hints>
              (6)    -------+
      -}

      {- (1) -} header <> colon <+> align (annotated message)
        <> {- (2), (3), (4) -} fold (uncurry (prettySubReport fileContent withUnicode isError tabSize maxLineNumberLength) <$> groupedMarkers)
        <> {- (5) -} ( if
                           | null hints && null markers -> mempty
                           | null hints -> mempty
                           | otherwise -> hardline <+> dotPrefix maxLineNumberLength withUnicode
                     )
        <> prettyAllHints hints maxLineNumberLength withUnicode
        <> hardline
        <> {- (6) -} ( if null markers && null hints
                         then mempty
                         else
                           annotate RuleColor (pad (maxLineNumberLength + 2) (unicode '-' '─' withUnicode) mempty <> unicode "+" "╯" withUnicode)
                             <> hardline
                     )

-------------------------------------------------------------------------------------
----- INTERNAL STUFF ----------------------------------------------------------------
-------------------------------------------------------------------------------------

-- | Inserts a given number of character after a 'Doc'ument.
pad :: Int -> Char -> Doc ann -> Doc ann
pad n c d = width d \w -> pretty $ replicate (n - w) c

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
  WithUnicode ->
  Doc (Annotation ann)
dotPrefix leftLen withUnicode =
  pad leftLen ' ' mempty
    <+> annotate RuleColor (unicode ":" "•" withUnicode)
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
  WithUnicode ->
  Doc (Annotation ann)
pipePrefix leftLen withUnicode = pad leftLen ' ' mempty <+> annotate RuleColor (unicode "|" "│" withUnicode)
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
  WithUnicode ->
  Doc (Annotation ann)
linePrefix leftLen lineNo withUnicode =
  let lineNoLen = length (show lineNo)
   in annotate RuleColor $ mempty <+> pad (leftLen - lineNoLen) ' ' mempty <> pretty lineNo <+> unicode "|" "│" withUnicode
{-# INLINE linePrefix #-}

-- | Creates an ellipsis-prefix, when some line numbers are not consecutive.
--
--   Pretty printing yields those results:
--
--   [with unicode] "@␣␣␣␣␣⋮␣@"
--   [without unicode] "@␣␣␣␣...@"
ellipsisPrefix ::
  Int ->
  WithUnicode ->
  Doc (Annotation ann)
ellipsisPrefix leftLen withUnicode = pad leftLen ' ' mempty <> annotate RuleColor (unicode "..." (space <> "⋮") withUnicode)

groupMarkersPerFile ::
  [(Position, Marker msg)] ->
  [(Bool, [(Position, Marker msg)])]
groupMarkersPerFile [] = []
groupMarkersPerFile markers =
  let markersPerFile = List.foldl' (HashMap.unionWith (<>)) mempty $ markers <&> \tup@(p, _) -> HashMap.singleton (file p) [tup]
   in -- put all markers on the same file together
      -- NOTE: it's a shame that `HashMap.unionsWith f = foldl' (HashMap.unionWith f) mempty` does not exist

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

-- | Prettyprint a sub-report, which is a part of the report spanning across a single file
prettySubReport ::
  -- | The content of files in the diagnostics
  FileMap ->
  -- | Is the output done with Unicode characters?
  WithUnicode ->
  -- | Is the current report an error report?
  Bool ->
  -- | The number of spaces each TAB character will span
  TabSize ->
  -- | The size of the biggest line number
  Int ->
  -- | Is this sub-report the first one in the list?
  Bool ->
  -- | The list of line-ordered markers appearing in a single file
  [(Position, Marker (Doc ann))] ->
  Doc (Annotation ann)
prettySubReport fileContent withUnicode isError tabSize maxLineNumberLength isFirst markers =
  let (markersPerLine, multilineMarkers) = splitMarkersPerLine markers
      -- split the list on whether markers are multiline or not

      sortedMarkersPerLine = {- second (List.sortOn (first $ snd . begin)) <$> -} List.sortOn fst (HashMap.toList markersPerLine)

      reportFile = maybe (pretty @Position def) (pretty . fst) $ List.safeHead (List.sortOn (Down . snd) markers)
      -- the reported file is the file of the first 'This' marker (only one must be present)

      allLineNumbers = List.sort $ List.nub $ (fst <$> sortedMarkersPerLine) <> (multilineMarkers >>= \(Position (bl, _) (el, _) _, _) -> [bl .. el])

      fileMarker =
        ( if isFirst
            then
              space <> pad maxLineNumberLength ' ' mempty
                <+> annotate RuleColor (unicode "+-->" "╭──▶" withUnicode)
            else
              space <> dotPrefix maxLineNumberLength withUnicode <> hardline
                <> annotate RuleColor (pad (maxLineNumberLength + 2) (unicode '-' '─' withUnicode) mempty)
                <> annotate RuleColor (unicode "+-->" "┼──▶" withUnicode)
        )
          <+> annotate FileColor reportFile
   in {- (2) -} hardline <> fileMarker
        <> hardline
          <+> {- (3) -}  {- (3) -} pipePrefix maxLineNumberLength withUnicode
        <> {- (4) -} prettyAllLines fileContent withUnicode isError tabSize maxLineNumberLength sortedMarkersPerLine multilineMarkers allLineNumbers

isThisMarker :: Marker msg -> Bool
isThisMarker (This _) = True
isThisMarker _ = False

-- |
splitMarkersPerLine :: [(Position, Marker msg)] -> (HashMap Int [(Position, Marker msg)], [(Position, Marker msg)])
splitMarkersPerLine [] = (mempty, mempty)
splitMarkersPerLine (m@(Position {..}, _) : ms) =
  let (bl, _) = begin
      (el, _) = end
   in (if bl == el then first (HashMap.insertWith (<>) bl [m]) else second (m :))
        (splitMarkersPerLine ms)

-- |
prettyAllLines ::
  FileMap ->
  WithUnicode ->
  Bool ->
  -- | The number of spaces each TAB character will span
  TabSize ->
  Int ->
  [(Int, [(Position, Marker (Doc ann))])] ->
  [(Position, Marker (Doc ann))] ->
  [Int] ->
  Doc (Annotation ann)
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
            <> (if l2 /= l1 + 1 then hardline <+> dotPrefix leftLen withUnicode else mempty)
            <> prettyAllLines files withUnicode isError tabSize leftLen inline ms (l2 : ls)
  where
    showForLine isLastLine line =
      {-
          A line of code is composed of:
          (1)     <line> | <source code>
          (2)            : <markers>
          (3)            : <marker messages>

          Multline markers may also take additional space (2 characters) on the right of the bar
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
                    then colorOfFirstMultilineMarker (unicode "|  " "│  " withUnicode)
                    else "   "
                else mempty
            (p@(Position _ (el, _) _), marker) : _ ->
              let hasPredecessor = el == line || maybe False ((/=) p . fst . fst) (List.safeUncons multiline)
               in colorOfFirstMultilineMarker
                    (unicode (bool "+" "|" hasPredecessor ) (bool "╭" "├" hasPredecessor) withUnicode)
                    <> annotate (markerColor isError marker) (unicode ">" "┤" withUnicode)
                    <> space

          -- we need to remove all blank markers because they are irrelevant to the display
          allInlineMarkersInLine' = filter ((/=) Blank . snd) allInlineMarkersInLine
          allMultilineMarkersSpanningLine' = filter ((/=) Blank . snd) allMultilineMarkersSpanningLine

          (widths, renderedCode) = getLine_ files (allInlineMarkersInLine <> allMultilineMarkersInLine <> allMultilineMarkersSpanningLine') line tabSize isError
       in ( otherMultilines,
            hardline
              <> {- (1) -} linePrefix leftLen line withUnicode <+>  additionalPrefix
              <> renderedCode
              <> {- (2) -} showAllMarkersInLine (not $ null multiline) inSpanOfMultiline colorOfFirstMultilineMarker withUnicode isError leftLen widths allInlineMarkersInLine'
              <> showMultiline (isLastLine || List.safeLast multilineEndingOnLine == List.safeLast multiline) multilineEndingOnLine
          )

    showMultiline _ [] = mempty
    showMultiline isLastMultiline multiline =
      let colorOfFirstMultilineMarker = markerColor isError . snd <$> List.safeHead multiline
          -- take the color of the last multiline marker in case we need to add additional bars

          prefix = space <> dotPrefix leftLen withUnicode <> space

          prefixWithBar color = prefix <> maybe id annotate color (unicode "| " "│ " withUnicode)

          showMultilineMarkerMessage (_, Blank) _ = mempty
          showMultilineMarkerMessage (_, marker) isLast =
            annotate (markerColor isError marker) $
              ( if isLast && isLastMultiline
                  then unicode "`- " "╰╸ " withUnicode
                  else unicode "|- " "├╸ " withUnicode
              )
                <> replaceLinesWith (if isLast then prefix <> "   " else prefixWithBar (Just $ markerColor isError marker) <> space) 0 (annotated $ markerMessage marker)

          showMultilineMarkerMessages [] = []
          showMultilineMarkerMessages [m] = [showMultilineMarkerMessage m True]
          showMultilineMarkerMessages (m : ms) = showMultilineMarkerMessage m False : showMultilineMarkerMessages ms
       in hardline <> prefixWithBar colorOfFirstMultilineMarker <> hardline <> prefix <> fold (List.intersperse (hardline <> prefix) $ showMultilineMarkerMessages multiline)

-- |
getLine_ ::
  FileMap ->
  [(Position, Marker msg)] ->
  Int ->
  TabSize ->
  Bool ->
  (WidthTable, Doc (Annotation ann))
getLine_ files markers line (TabSize tabSize) isError =
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

-- |
showAllMarkersInLine :: Bool -> Bool -> (Doc (Annotation ann) -> Doc (Annotation ann)) -> WithUnicode -> Bool -> Int -> WidthTable -> [(Position, Marker (Doc ann))] -> Doc (Annotation ann)
showAllMarkersInLine _ _ _ _ _ _ _ [] = mempty
showAllMarkersInLine hasMultilines inSpanOfMultiline colorMultilinePrefix withUnicode isError leftLen widths ms =
  let maxMarkerColumn = snd $ end $ fst $ List.last $ List.sortOn (snd . end . fst) ms
      specialPrefix
        | inSpanOfMultiline = colorMultilinePrefix (unicode "| " "│ " withUnicode) <> space
        | hasMultilines = colorMultilinePrefix "  " <> space
        | otherwise = mempty
   in -- get the maximum end column, so that we know when to stop looking for other markers on the same line
      hardline <+> dotPrefix leftLen withUnicode <+> (if List.null ms then mempty else specialPrefix <> showMarkers 1 maxMarkerColumn <> showMessages specialPrefix ms maxMarkerColumn)
  where
    widthAt i = 0 `fromMaybe` safeArrayIndex i widths
    widthsBetween start end =
      sum $ take (end - start) $ drop (start - 1) $ Array.elems widths

    showMarkers n lineLen
      | n > lineLen = mempty -- reached the end of the line
      | otherwise =
        let allMarkers = flip filter ms \(Position (_, bc) (_, ec) _, mark) -> mark /= Blank && n >= bc && n < ec
         in -- only consider markers which span onto the current column
            case allMarkers of
              [] -> fold (replicate (widthAt n) space) <> showMarkers (n + 1) lineLen
              (Position {..}, marker) : _ ->
                annotate
                  (markerColor isError marker)
                  ( if snd begin == n
                      then unicode "^" "┬" withUnicode <> fold (replicate (widthAt n - 1) (unicode "-" "─" withUnicode))
                      else fold (replicate (widthAt n) (unicode "-" "─" withUnicode))
                  )
                  <> showMarkers (n + 1) lineLen

    showMessages specialPrefix ms lineLen = case List.safeUncons ms of
      Nothing -> mempty -- no more messages to show
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

                  pipesBeforeRendered = pipesBefore <&> second \marker -> annotate (markerColor isError marker) (unicode "|" "│" withUnicode)
                  -- pre-render pipes which are before because they will be shown

                  lastBeginPosition = snd . begin . fst <$> List.safeLast (List.sortOn (snd . begin . fst) pipesAfter)

                  lineLen = case lastBeginPosition of
                    Nothing -> 0
                    Just col -> widthsBetween bc col

                  currentPipe = unicode (bool "`" "|" hasSuccessor) (bool "╰" "├" hasSuccessor) withUnicode

                  lineChar = unicode '-' '─' withUnicode
                  pointChar = unicode "-" "╸" withUnicode

                  bc' = bc + lineLen + 2
                  pipesBeforeMessageStart = List.filter ((< bc') . snd . begin . fst) pipesAfter
                  -- consider pipes before, as well as pipes which came before the text rectangle bounds
                  pipesBeforeMessageRendered = (pipesBefore <> pipesBeforeMessageStart) <&> second \marker -> annotate (markerColor isError marker) (unicode "|" "│" withUnicode)
               in -- also pre-render pipes which are before the message text bounds, because they will be shown if the message is on
                  -- multiple lines

                  lineStart pipesBeforeRendered
                    <> annotate (markerColor isError msg) (currentPipe <> pretty (replicate lineLen lineChar) <> pointChar)
                    <+> annotate (markerColor isError msg) (replaceLinesWith (space <> lineStart pipesBeforeMessageRendered <+> if List.null pipesBeforeMessageStart then "  " else " ") 0 $ annotated $ markerMessage msg)
         in hardline <+> prefix <> showMessages specialPrefix pipes lineLen

-- WARN: uses the internal of the library
--
--       DO NOT use a wildcard here, in case the internal API exposes one more constructor
replaceLinesWith :: Doc ann -> Int -> Doc ann -> Doc ann
replaceLinesWith repl = go
 where
  replWidth = sdsWidth . layoutCompact $ repl
  sdsWidth = \case
    SFail -> 0
    SEmpty -> 0
    SChar _ sds -> 1 + sdsWidth sds
    SText l _ sds -> l + sdsWidth sds
    SLine _ _ -> error "replaceLinesWith was given a prefix with a line break"
    SAnnPush _ sds -> sdsWidth sds
    SAnnPop sds -> sdsWidth sds
  replWithNesting n = hardline <> repl <> pretty (textSpaces n)
  go n = \case
    Line -> replWithNesting n
    Fail -> Fail
    Empty -> Empty
    Char c -> Char c
    Text l txt -> Text l txt
    FlatAlt f d -> FlatAlt (go n f) (go n d)
    Cat c d -> Cat (go n c) (go n d)
    Nest n' d -> go (n + n') d
    Union c d -> Union (go n c) (go n d)
    Column f -> Column (go n . f)
    -- In this case we add both our fake nesting level (from the 'Nest'
    -- constructors we've eliminated) and the nesting level from the line
    -- prefixes
    Nesting f -> Nesting (go n . f . (+ replWidth) . (+ n))
    Annotated ann doc -> Annotated ann (go n doc)
    WithPageWidth f -> WithPageWidth (go n . f)

-- | Extracts the color of a marker as a 'Doc' coloring function.
markerColor ::
  -- | Whether the marker is in an error context or not.
  --   This really makes a difference for a 'This' marker.
  Bool ->
  -- | The marker to extract the color from.
  Marker msg ->
  -- | A function used to color a 'Doc'.
  Annotation ann
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
prettyAllHints :: [Note (Doc ann)] -> Int -> WithUnicode -> Doc (Annotation ann)
prettyAllHints [] _ _ = mempty
prettyAllHints (h : hs) leftLen withUnicode =
  {-
        A hint is composed of:
        (1)         : Hint: <hint message>
  -}
  let prefix = space <> pipePrefix leftLen withUnicode
   in hardline <> prefix <+> annotate HintColor (notePrefix h <+> replaceLinesWith prefix 7 (annotated $ noteMessage h))
        <> prettyAllHints hs leftLen withUnicode
  where
    notePrefix (Note _) = "Note:"
    notePrefix (Hint _) = "Hint:"

    noteMessage (Note msg) = msg
    noteMessage (Hint msg) = msg

safeArrayIndex :: (Ix i, IArray a e) => i -> a i e -> Maybe e
safeArrayIndex i a
  | Array.inRange (Array.bounds a) i = Just (a ! i)
  | otherwise = Nothing

annotated :: Doc ann -> Doc (Annotation ann)
annotated = reAnnotate OtherStyle

unicode :: a -> a -> WithUnicode -> a
unicode f t = \case
  WithoutUnicode -> f
  WithUnicode -> t
