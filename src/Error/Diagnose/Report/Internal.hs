{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -Wno-name-shadowing #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Error.Diagnose.Report.Internal
-- Description : Internal workings for report definitions and pretty printing.
-- Copyright   : (c) Mesabloo, 2021
-- License     : BSD3
-- Stability   : experimental
-- Portability : Portable
--
-- /Warning/: The API of this module can break between two releases, therefore you should not rely on it.
--            It is also highly undocumented.
--
--            Please limit yourself to the "Error.Diagnose.Report" module, which exports some of the useful functions defined here.
module Error.Diagnose.Report.Internal where

#ifdef USE_AESON
import Data.Aeson (ToJSON(..), object, (.=))
#endif
import Control.Applicative ((<|>))
import Data.Bifunctor (bimap, first, second)
import Data.Char.WCWidth (wcwidth)
import Data.Default (def)
import Data.Foldable (fold)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.HashMap.Lazy as IntMap
import qualified Data.List as List
import qualified Data.List.Safe as List
import Data.Ord (Down (Down))
import Error.Diagnose.Position
import Prettyprinter (Doc, Pretty (..), align, annotate, colon, hardline, lbracket, rbracket, space, width, (<+>))
import Prettyprinter.Internal (Doc (..))
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..), bold, color, colorDull)

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
      [msg]
      -- ^ A list of hints to add at the end of the report.

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
#endif

-- | The type of markers with abstract message type, shown under code lines.
data Marker msg
  = -- | A red or yellow marker under source code, marking important parts of the code.
    This msg
  | -- | A blue marker symbolizing additional information.
    Where msg
  | -- | A magenta marker to report potential fixes.
    Maybe msg

instance Eq (Marker msg) where
  This _ == This _ = True
  Where _ == Where _ = True
  Maybe _ == Maybe _ = True
  _ == _ = False
  {-# INLINEABLE (==) #-}

instance Ord (Marker msg) where
  This _ < _ = False
  Where _ < This _ = True
  Where _ < _ = False
  Maybe _ < _ = True
  {-# INLINEABLE (<) #-}

  m1 <= m2 = m1 < m2 || m1 == m2
  {-# INLINEABLE (<=) #-}

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
    [msg] ->
    Report msg
warn = Report False
{-# INLINE warn #-}
err = Report True
{-# INLINE err #-}

-- | Pretty prints a report to a 'Doc' handling colors.
prettyReport ::
  Pretty msg =>
  -- | The content of the file the reports are for
  HashMap FilePath [String] ->
  -- | Should we print paths in unicode?
  Bool ->
  -- | The number of spaces each TAB character will span
  Int ->
  -- | The whole report to output
  Report msg ->
  Doc AnsiStyle
prettyReport fileContent withUnicode tabSize (Report isError code message markers hints) =
  let sortedMarkers = List.sortOn (fst . begin . fst) markers
      -- sort the markers so that the first lines of the reports are the first lines of the file

      groupedMarkers = groupMarkersPerFile sortedMarkers
      -- group markers by the file they appear in, and put `This` markers at the top of the report

      maxLineNumberLength = maybe 3 (max 3 . length . show . fst . end . fst) $ List.safeLast markers
      -- if there are no markers, then default to 3, else get the maximum between 3 and the length of the last marker

      header =
        annotate
          (bold <> color if isError then Red else Yellow)
          ( lbracket
              <> ( if isError
                     then "error"
                     else "warning"
                 )
              <> case code of
                Nothing -> rbracket
                Just code -> space <> pretty code <> rbracket
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

      {- (1) -} header <> colon <+> align (pretty message)
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
                           annotate (bold <> color Black) (pad (maxLineNumberLength + 2) (if withUnicode then '─' else '-') mempty <> if withUnicode then "╯" else "+")
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
  Bool ->
  Doc AnsiStyle
dotPrefix leftLen withUnicode = pad leftLen ' ' mempty <+> annotate (bold <> color Black) (if withUnicode then "•" else ":")
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
  Doc AnsiStyle
pipePrefix leftLen withUnicode = pad leftLen ' ' mempty <+> annotate (bold <> color Black) (if withUnicode then "│" else "|")
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
  Doc AnsiStyle
linePrefix leftLen lineNo withUnicode =
  let lineNoLen = length (show lineNo)
   in annotate (bold <> color Black) $ mempty <+> pad (leftLen - lineNoLen) ' ' mempty <> pretty lineNo <+> if withUnicode then "│" else "|"
{-# INLINE linePrefix #-}

groupMarkersPerFile ::
  Pretty msg =>
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
  Pretty msg =>
  -- | The content of files in the diagnostics
  HashMap FilePath [String] ->
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
  Doc AnsiStyle
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
                <+> annotate (bold <> color Black) (if withUnicode then "╭──▶" else "+-->")
            else
              space <> dotPrefix maxLineNumberLength withUnicode <> hardline
                <> annotate (bold <> color Black) (pad (maxLineNumberLength + 2) (if withUnicode then '─' else '-') mempty)
                <> annotate (bold <> color Black) (if withUnicode then "┼──▶" else "+-->")
        )
          <+> annotate (bold <> colorDull Green) reportFile
   in {- (2) -} hardline <> fileMarker
        <> hardline
          <+> {- (3) -} pipePrefix maxLineNumberLength withUnicode
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
  Pretty msg =>
  HashMap FilePath [String] ->
  Bool ->
  Bool ->
  -- | The number of spaces each TAB character will span
  Int ->
  Int ->
  [(Int, [(Position, Marker msg)])] ->
  [(Position, Marker msg)] ->
  [Int] ->
  Doc AnsiStyle
prettyAllLines _ _ _ _ _ _ [] [] = mempty
prettyAllLines _ withUnicode isError _ leftLen _ multiline [] =
  let colorOfLastMultilineMarker = maybe mempty (markerColor isError . snd) (List.safeLast multiline)
      -- take the color of the last multiline marker in case we need to add additional bars

      prefix = hardline <+> dotPrefix leftLen withUnicode <> space
      prefixWithBar color = prefix <> annotate color (if withUnicode then "│ " else "| ")

      showMultilineMarkerMessage (_, marker) isLast =
        annotate (markerColor isError marker) $
          ( if isLast
              then if withUnicode then "╰╸ " else "`- "
              else if withUnicode then "├╸ " else "|- "
          )
            <> replaceLinesWith (if isLast then prefix <> "   " else prefixWithBar (markerColor isError marker) <> space) (pretty $ markerMessage marker)

      showMultilineMarkerMessages [] = []
      showMultilineMarkerMessages [m] = [showMultilineMarkerMessage m True]
      showMultilineMarkerMessages (m : ms) = showMultilineMarkerMessage m False : showMultilineMarkerMessages ms
   in prefixWithBar colorOfLastMultilineMarker <> prefix <> fold (List.intersperse prefix $ showMultilineMarkerMessages $ reverse multiline)
prettyAllLines files withUnicode isError tabSize leftLen inline multiline (line : ls) =
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

      !additionalPrefix = case allMultilineMarkersInLine of
        [] ->
          if not $ null multiline
            then
              if not $ null allMultilineMarkersSpanningLine
                then colorOfFirstMultilineMarker if withUnicode then "│  " else "|  "
                else "   "
            else mempty
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

      allMarkersInLine = {- List.sortOn fst $ -} allInlineMarkersInLine <> allMultilineMarkersInLine

      (widths, renderedCode) = getLine_ files (allMarkersInLine <> allMultilineMarkersSpanningLine) line tabSize isError
   in hardline
        <> {- (1) -} linePrefix leftLen line withUnicode <+> additionalPrefix
        <> renderedCode
        <> {- (2) -} showAllMarkersInLine (not $ null multiline) inSpanOfMultiline colorOfFirstMultilineMarker withUnicode isError leftLen widths allInlineMarkersInLine
        <> {- (3) -} prettyAllLines files withUnicode isError tabSize leftLen inline multiline ls

-- |
getLine_ :: HashMap FilePath [String] -> [(Position, Marker msg)] -> Int -> Int -> Bool -> (IntMap.HashMap Int Int, Doc AnsiStyle)
getLine_ files markers line tabSize isError = case List.safeIndex (line - 1) =<< (HashMap.!?) files . file . fst =<< List.safeHead markers of
  Nothing -> (mempty, annotate (bold <> colorDull Magenta) "<no line>")
  Just code ->
    let (tabs, code') = indexedWithTabsReplaced code
     in ( tabs,
          fold $
            code' <&> \(n, c) ->
              let colorizingMarkers = flip
                    filter
                    markers
                    \(Position (bl, bc) (el, ec) _, _) ->
                      if bl == el
                        then n >= bc && n < ec
                        else (bl == line && n >= bc) || (el == line && n < ec)
               in maybe id ((\m -> annotate (bold <> markerColor isError m)) . snd) (List.safeHead colorizingMarkers) (pretty c)
        )
  where
    indexedWithTabsReplaced :: String -> (IntMap.HashMap Int Int, [(Int, Char)])
    indexedWithTabsReplaced = goIndexed 1

    goIndexed :: Int -> String -> (IntMap.HashMap Int Int, [(Int, Char)])
    goIndexed _ [] = (mempty, [])
    goIndexed n ('\t' : xs) = bimap (IntMap.insert n tabSize) (replicate tabSize (n, ' ') <>) (goIndexed (n + 1) xs)
    goIndexed n (x : xs) = bimap (IntMap.insert n (wcwidth x)) ((n, x) :) (goIndexed (n + 1) xs)

-- |
showAllMarkersInLine :: Pretty msg => Bool -> Bool -> (Doc AnsiStyle -> Doc AnsiStyle) -> Bool -> Bool -> Int -> IntMap.HashMap Int Int -> [(Position, Marker msg)] -> Doc AnsiStyle
showAllMarkersInLine _ _ _ _ _ _ _ [] = mempty
showAllMarkersInLine hasMultilines inSpanOfMultiline colorMultilinePrefix withUnicode isError leftLen widths ms =
  let maxMarkerColumn = snd $ end $ fst $ List.last $ List.sortOn (snd . end . fst) ms
      specialPrefix
        | inSpanOfMultiline = colorMultilinePrefix (if withUnicode then "│ " else "| ") <> space
        | hasMultilines = colorMultilinePrefix "  " <> space
        | otherwise = mempty
   in -- get the maximum end column, so that we know when to stop looking for other markers on the same line
      hardline <+> dotPrefix leftLen withUnicode <+> (if List.null ms then mempty else specialPrefix <> showMarkers 1 maxMarkerColumn <> showMessages specialPrefix ms maxMarkerColumn)
  where
    showMarkers n lineLen
      | n > lineLen = mempty -- reached the end of the line
      | otherwise =
        let allMarkers = flip filter ms \(Position (_, bc) (_, ec) _, _) -> n >= bc && n < ec
         in -- only consider markers which span onto the current column
            case allMarkers of
              [] -> fold (replicate (IntMap.lookupDefault 0 n widths) space) <> showMarkers (n + 1) lineLen
              (Position {..}, marker) : _ ->
                annotate
                  (markerColor isError marker)
                  ( if snd begin == n
                      then (if withUnicode then "┬" else "^") <> fold (replicate (IntMap.lookupDefault 0 n widths - 1) if withUnicode then "─" else "-")
                      else fold (replicate (IntMap.lookupDefault 0 n widths) if withUnicode then "─" else "-")
                  )
                  <> showMarkers (n + 1) lineLen

    showMessages specialPrefix ms lineLen = case List.safeUncons ms of
      Nothing -> mempty -- no more messages to show
      Just ((Position b@(_, bc) _ _, msg), pipes) ->
        let filteredPipes = filter ((/= b) . begin . fst) pipes
            -- record only the pipes corresponding to markers on different starting positions
            nubbedPipes = List.nubBy ((==) `on` (begin . fst)) filteredPipes
            -- and then remove all duplicates

            allColumns _ [] = (1, [])
            allColumns n ms@((Position (_, bc) _ _, col) : ms')
              | n == bc = bimap (+ 1) (col :) (allColumns (n + 1) ms')
              | n < bc = bimap (+ 1) (replicate (IntMap.lookupDefault 0 n widths) space <>) (allColumns (n + 1) ms)
              | otherwise = bimap (+ 1) (replicate (IntMap.lookupDefault 0 n widths) space <>) (allColumns (n + 1) ms')
            -- transform the list of remaining markers into a single document line

            hasSuccessor = length filteredPipes /= length pipes

            lineStart pipes =
              let (n, docs) = allColumns 1 $ List.sortOn (snd . begin . fst) pipes
                  numberOfSpaces = sum [IntMap.lookupDefault 0 x widths | x <- [n .. bc - 1]] -- bc - n
               in dotPrefix leftLen withUnicode <+> specialPrefix <> fold docs <> pretty (replicate numberOfSpaces ' ')
            -- the start of the line contains the "dot"-prefix as well as all the pipes for all the still not rendered marker messages

            prefix =
              let (pipesBefore, pipesAfter) = List.partition ((< bc) . snd . begin . fst) nubbedPipes
                  -- split the list so that all pipes before can have `|`s but pipes after won't

                  pipesBeforeRendered = pipesBefore <&> second \marker -> annotate (markerColor isError marker) (if withUnicode then "│" else "|")
                  -- pre-render pipes which are before because they will be shown

                  lastBeginPosition = snd . begin . fst <$> List.safeHead (List.sortOn (snd . begin . fst) pipesAfter)

                  lineLen = case lastBeginPosition of
                    Nothing -> 0
                    Just col -> sum [IntMap.lookupDefault 0 x widths | x <- [bc .. col - 1]]

                  currentPipe =
                    if
                        | withUnicode && hasSuccessor -> "├"
                        | withUnicode -> "╰"
                        | hasSuccessor -> "|"
                        | otherwise -> "`"

                  lineChar = if withUnicode then '─' else '-'
                  pointChar = if withUnicode then "╸" else "-"
               in lineStart pipesBeforeRendered
                    <> annotate (markerColor isError msg) (currentPipe <> pretty (replicate lineLen lineChar) <> pointChar)
                    <+> annotate (markerColor isError msg) (replaceLinesWith (hardline <+> lineStart pipesBeforeRendered <+> "  ") $ pretty $ markerMessage msg)
         in hardline <+> prefix <> showMessages specialPrefix pipes lineLen

-- WARN: uses the internal of the library
--
--       DO NOT use a wildcard here, in case the internal API exposes one more constructor

-- |
replaceLinesWith :: Doc ann -> Doc ann -> Doc ann
replaceLinesWith repl Line = repl
replaceLinesWith _ Fail = Fail
replaceLinesWith _ Empty = Empty
replaceLinesWith _ (Char c) = Char c
replaceLinesWith _ (Text n s) = Text n s
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
  AnsiStyle
markerColor isError (This _) = if isError then color Red else color Yellow
markerColor _ (Where _) = colorDull Blue
markerColor _ (Maybe _) = color Magenta
{-# INLINE markerColor #-}

-- | Retrieves the message held by a marker.
markerMessage :: Marker msg -> msg
markerMessage (This m) = m
markerMessage (Where m) = m
markerMessage (Maybe m) = m
{-# INLINE markerMessage #-}

-- | Pretty prints all hints.
prettyAllHints :: Pretty msg => [msg] -> Int -> Bool -> Doc AnsiStyle
prettyAllHints [] _ _ = mempty
prettyAllHints (h : hs) leftLen withUnicode =
  {-
        A hint is composed of:
        (1)         : Hint: <hint message>
  -}
  let prefix = hardline <+> pipePrefix leftLen withUnicode
   in prefix <+> annotate (color Cyan) (annotate bold "Hint:" <+> replaceLinesWith (prefix <+> "      ") (pretty h))
        <> prettyAllHints hs leftLen withUnicode
