{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- |
-- Module      : Error.Diagnose.Layout.Ariadne
-- Description : Main description of the Ariadne layout.
-- Copyright   : (c) Mesabloo and contributors, 2022-
-- License     : BSD3
-- Stability   : experimental
-- Portability : Portable
module Error.Diagnose.Layout.Ariadne (ariadneLayout) where

import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Monad (forM_, unless, when)
import Control.Monad.Trans.Writer (Writer, execWriter, tell)
import qualified Data.Array.IArray as Array
import Data.Bifunctor (bimap, first, second)
import Data.Foldable (fold)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.List as List
import Data.Maybe (fromMaybe, isJust)
import Data.Some (Some (..), withSome, withSomeM)
import Error.Diagnose.Diagnostic (def, filesOf, reportsOf)
import Error.Diagnose.Layout (FileMap, Layout)
import Error.Diagnose.Layout.Ariadne.Config (Configuration (..), configuration)
import Error.Diagnose.Layout.Ariadne.Style (AriadneAnnotation (..))
import Error.Diagnose.Position (SourceRange (..))
import Error.Diagnose.Pretty (Doc, Pretty, align, annotate, colon, hardline, lbracket, pretty, rbracket, space, width, (<+>))
import Error.Diagnose.Report (Marker (..), MarkerKind (..), Note (..), Report (..), Severity (..))
import Error.Diagnose.Utils (WidthTable, fetchLine, markerMessage, markerPosition, replaceLinesWith, safeArrayIndex)
import qualified Safe as List (headMay, lastMay)

-- | Pretty prints a 'Error.Diagnose.Diagnostic.Diagnostic' into a 'Doc'ument that can be output using 'Error.Diagnose.Pretty.hPutDoc'.
--   The generated 'Doc'ument should resemble Ariadne's error style.
--
--   Colors are put by default.
--   If you do not want these, just 'Error.Diagnose.Pretty.unAnnotate' the resulting document like so:
--
--   >>> let doc = unAnnotate (ariadneLayout withUnicode tabSize diagnostic)
ariadneLayout :: Layout msg AriadneAnnotation
ariadneLayout withUnicode tabSize diag = fold . List.intersperse hardline $ go <$> reportsOf diag
  where
    go rep = execWriter $ prettyReport (filesOf diag) (configuration withUnicode) tabSize rep
{-# INLINE ariadneLayout #-}

--------------------------------------
------------- INTERNAL ---------------
--------------------------------------

-- | A simple type alias for a function generating a 'Doc' 'AriadneAnnotation' with return value of type 'a'.
type Layouter a = Writer (Doc AriadneAnnotation) a

-- | Pretty prints a report to a 'Doc' handling colors.
prettyReport ::
  Pretty msg =>
  -- | The content of the file the reports are for
  FileMap ->
  -- | Should we print paths in unicode?
  Configuration ->
  -- | The number of spaces each TAB character will span
  Int ->
  -- | The whole report to output
  Report msg ->
  Layouter ()
prettyReport fileContent conf tabSize (Report sev code message markers hints) =
  prettyReport' fileContent conf tabSize sev code message markers hints

-- | Pretty prints an unwrapped report to a 'Doc' handling colors.
prettyReport' ::
  Pretty msg =>
  -- | The content of the files.
  FileMap ->
  -- | A configuration for Unicode/ASCII characters.
  Configuration ->
  -- | The number of spaces that a TAB character must span.
  Int ->
  -- | The severity of the report.
  Severity ->
  -- | The optional error code.
  Maybe msg ->
  -- | The main message of the report.
  msg ->
  -- | Every marker present in the main section of the report.
  [Marker msg 'MainMarker] ->
  -- | All notes present at the end.
  [Note msg] ->
  Layouter ()
prettyReport' fileContent conf tabSize sev code message markers hints = do
  let markers' = (markerPosition &&& id) <$> markers

      sortedMarkers = List.sortOn (fst . begin . fst) markers'
      -- sort the markers so that the first lines of the reports are the first lines of the file

      groupedMarkers = groupMarkersPerFile (snd <$> sortedMarkers)
      -- group markers by the file they appear in, and put `This` markers at the top of the report

      maxLineNumberLength = maybe 3 (max 3 . length . show . fst . end . fst) $ List.lastMay markers'
  -- if there are no markers, then default to 3, else get the maximum between 3 and the length of the last marker

  {-
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

  {- (1) -}
  tell $ prettySeverity sev code
  tell $ colon <> space
  tell . align $ pretty message

  {- (2), (3), (4) -}
  forM_ groupedMarkers (uncurry (prettySubReport fileContent conf sev tabSize maxLineNumberLength) . second (fmap Some))

  {- (5) -}
  when (not (null hints) && not (null markers)) do
    tell $ hardline <> space
    tell $ dotPrefix maxLineNumberLength conf

  prettyAllHints hints maxLineNumberLength fileContent conf sev tabSize
  when (null hints) do
    tell $ hardline <> space

  {- (6) -}
  unless (null markers && null hints) do
    tell $ pipePrefix maxLineNumberLength conf
    tell hardline
    tell . annotate Rule $ pad (maxLineNumberLength + 2) (horizontalRule conf) mempty <> pretty (endRuleSuffix conf)
    tell hardline

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
  Configuration ->
  Doc AriadneAnnotation
dotPrefix leftLen conf = pad leftLen ' ' mempty <+> annotate Rule (pretty $ dotRule conf)
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
  Configuration ->
  Doc AriadneAnnotation
pipePrefix leftLen conf = pad leftLen ' ' mempty <+> annotate Rule (pretty $ verticalRule conf)
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
  Configuration ->
  Doc AriadneAnnotation
linePrefix leftLen lineNo conf =
  let lineNoLen = length (show lineNo)
   in annotate Rule (space <> pad (leftLen - lineNoLen) ' ' mempty)
        <> annotate LineNumberTint (pretty lineNo)
        <+> annotate Rule (pretty $ verticalRule conf)
{-# INLINE linePrefix #-}

groupMarkersPerFile ::
  Pretty msg =>
  [Marker msg 'MainMarker] ->
  [(Bool, [Marker msg 'MainMarker])]
groupMarkersPerFile [] = []
groupMarkersPerFile markers =
  let -- put all markers on the same file together
      -- NOTE: it's a shame that `HashMap.unionsWith f = foldl' (HashMap.unionWith f) mempty` does not exist
      markersPerFile = List.foldl' (HashMap.unionWith (<>)) mempty $ markers <&> \m -> HashMap.singleton (file $ markerPosition m) [m]
   in onlyFirstToTrue $ putThisMarkersAtTop $ HashMap.elems markersPerFile
  where
    onlyFirstToTrue = go True []

    go _ acc [] = reverse acc
    go t acc (x : xs) = go False ((t, x) : acc) xs

    putThisMarkersAtTop = List.sortBy \ms1 ms2 ->
      if
          | any isPrimaryMarker ms1 -> LT
          | any isPrimaryMarker ms2 -> GT
          | otherwise -> EQ

-- | Prettyprint a sub-report, which is a part of the report spanning across a single file
prettySubReport ::
  Pretty msg =>
  -- | The content of files in the diagnostics
  FileMap ->
  -- | Is the output done with Unicode characters?
  Configuration ->
  -- | Is the current report an error report?
  Severity ->
  -- | The number of spaces each TAB character will span
  Int ->
  -- | The size of the biggest line number
  Int ->
  -- | Is this sub-report the first one in the list?
  Bool ->
  -- | The list of line-ordered markers appearing in a single file
  [Some (Marker msg)] ->
  Layouter ()
prettySubReport fileContent conf sev tabSize maxLineNumberLength isFirst markers = do
  let (markersPerLine, multilineMarkers) = splitMarkersPerLine markers
      -- split the list on whether markers are multiline or not

      sortedMarkersPerLine = List.sortOn fst (HashMap.toList markersPerLine)

      pos@(Range _ _ file) =
        maybe def (`withSome` markerPosition) $
          List.find (`withSome` isPrimaryMarker) markers <|> List.headMay markers
      reportFile = pretty pos
      -- the reported file is the file of the first 'This' marker (only one must be present)

      allLineNumbers =
        List.sort . List.nub $
          (fst <$> sortedMarkersPerLine)
            <> ( multilineMarkers >>= \m ->
                   let Range (bl, _) (el, _) _ = withSome m markerPosition
                    in [bl .. el]
               )

      onlyHasNotes = all (`withSome` isNoteMarker) markers

      fileMarker'
        | isFirst = do
            tell $ pad (maxLineNumberLength + 2) ' ' mempty
            tell . annotate Rule $
              pretty (startFileArrowPrefix conf)
                <> pad 2 (horizontalRule conf) mempty
                <> pretty (fileArrowTip conf)
        | otherwise = do
            unless onlyHasNotes do
              tell $ space <> dotPrefix maxLineNumberLength conf
              tell hardline
            tell . annotate Rule $ pad (maxLineNumberLength + 2) ' ' mempty
            tell . annotate Rule $
              pretty (inlineFileArrowPrefix conf)
                <> pad 2 (horizontalRule conf) mempty
                <> pretty (fileArrowTip conf)
      fileMarker = do
        fileMarker'
        tell $ space <> annotate File reportFile

  {- (2) -}
  tell hardline
  fileMarker
  tell $ hardline <> space

  {- (3) -}
  tell $ pipePrefix maxLineNumberLength conf

  {- (4) -}
  prettyAllLines fileContent conf sev file tabSize maxLineNumberLength sortedMarkersPerLine multilineMarkers allLineNumbers

-- | Checks whether a marker is a 'Primary' marker or not.
isPrimaryMarker :: Marker msg k -> Bool
isPrimaryMarker (Primary {}) = True
isPrimaryMarker _ = False

-- | Checks whether a marker can appear in a note report.
isNoteMarker :: Marker msg k -> Bool
isNoteMarker (AddCode {}) = True
isNoteMarker (RemoveCode {}) = True
isNoteMarker (Annotate {}) = True
isNoteMarker _ = False

splitMarkersPerLine :: [Some (Marker msg)] -> (HashMap Int [Some (Marker msg)], [Some (Marker msg)])
splitMarkersPerLine = go (mempty, mempty)
  where
    go acc [] = bimap (fmap reverse) reverse acc
    go acc (m : ms) =
      let Range (bl, _) (el, _) _ = withSome m markerPosition

          f
            | bl == el = first (HashMap.insertWith (<>) bl [m])
            | otherwise = second (m :)
       in go (f acc) ms

prettyAllLines ::
  Pretty msg =>
  FileMap ->
  Configuration ->
  Severity ->
  FilePath ->
  -- | The number of spaces each TAB character will span
  Int ->
  Int ->
  [(Int, [Some (Marker msg)])] ->
  [Some (Marker msg)] ->
  [Int] ->
  Layouter ()
prettyAllLines files conf sev file tabSize leftLen inline multiline lineNumbers =
  case lineNumbers of
    [] -> showMultiline True multiline
    [l] -> do
      ms <- showForLine True l
      prettyAllLines files conf sev file tabSize leftLen inline ms []
    l1 : l2 : ls -> do
      ms <- showForLine False l1
      when (l2 /= l1 + 1) do
        tell $ hardline <> space
        tell $ dotPrefix leftLen conf
      prettyAllLines files conf sev file tabSize leftLen inline ms (l2 : ls)
  where
    showForLine isLastLine line = do
      {-
          A line of code is composed of:
          (1)     <line> | <source code>
          (2)            : <markers>
          (3)            : <marker messages>

          Multline markers may also take additional space (2 characters) on the right of the bar
      -}
      let allInlineMarkersInLine = snd =<< filter ((==) line . fst) inline

          allMultilineMarkersInLine = flip filter multiline \m ->
            let Range (bl, _) (el, _) _ = withSome m markerPosition
             in bl == line || el == line

          allMultilineMarkersSpanningLine = flip filter multiline \m ->
            let Range (bl, _) (el, _) _ = withSome m markerPosition
             in bl < line && el > line

          inSpanOfMultiline = flip any multiline \m ->
            let Range (bl, _) (el, _) _ = withSome m markerPosition
             in bl <= line && el >= line

          colorOfFirstMultilineMarker = maybe id (annotate . (`withSome` markerColor sev)) (List.headMay $ allMultilineMarkersInLine <> allMultilineMarkersSpanningLine)
          -- take the first multiline marker to color the entire line, if there is one

          (multilineEndingOnLine, otherMultilines) = flip List.partition multiline \m ->
            let Range _ (el, _) _ = withSome m markerPosition
             in el == line

          !additionalPrefix = case allMultilineMarkersInLine of
            [] -> do
              unless (null multiline) do
                tell
                  if not $ null allMultilineMarkersSpanningLine
                    then colorOfFirstMultilineMarker (pretty $ verticalRule conf : "  ")
                    else "   "
            marker : _ -> do
              let p@(Range _ (el, _) _) = withSome marker markerPosition

                  hasPredecessor = el == line || maybe False ((/=) p . (`withSome` markerPosition) . fst) (List.uncons multiline)

                  color = withSome marker (markerColor sev)

              tell . colorOfFirstMultilineMarker . pretty $ (if hasPredecessor then multilineMarkerMiddle else multilineMarkerStart) conf
              tell . annotate color . pretty $ multilineMarkerHead conf
              tell space

          -- we need to remove all blank markers because they are irrelevant to the display
          allInlineMarkersInLine' = filter (`withSome` (not . isBlankMarker)) allInlineMarkersInLine
          allMultilineMarkersSpanningLine' = filter (`withSome` (not . isBlankMarker)) allMultilineMarkersSpanningLine

          (widths, _, renderedCode) = fetchLine files file line tabSize (NoLineTint, "<no line>") CodeTint (`withSome` markerColor sev) (allInlineMarkersInLine <> allMultilineMarkersInLine <> allMultilineMarkersSpanningLine')

          isLastMultiline = (==) `on` fmap (`withSome` markerPosition)

      tell hardline

      {- (1) -}
      tell $ linePrefix leftLen line conf <> space
      additionalPrefix
      tell renderedCode

      {- (2) -}
      showAllMarkersInLine (not $ null multiline) inSpanOfMultiline colorOfFirstMultilineMarker conf sev leftLen widths allInlineMarkersInLine'
      showMultiline (isLastLine || isLastMultiline (List.lastMay multilineEndingOnLine) (List.lastMay multiline)) multilineEndingOnLine

      pure otherMultilines

    showMultiline :: Pretty msg => Bool -> [Some (Marker msg)] -> Layouter ()
    showMultiline _ [] = pure ()
    showMultiline isLastMultiline multiline = do
      let colorOfFirstMultilineMarker = maybe id (annotate . (`withSome` markerColor sev)) $ List.headMay multiline
          -- take the color of the last multiline marker in case we need to add additional bars

          prefix = hardline <+> dotPrefix leftLen conf <> space

          prefixWithBar color = prefix <> color (pretty $ verticalRule conf : " ")

          showMultilineMarkerMessage :: Pretty msg => Marker msg k -> Bool -> Layouter ()
          showMultilineMarkerMessage (Blank {}) _ = pure ()
          showMultilineMarkerMessage marker isLast = do
            let color = markerColor sev marker

                prefix'
                  | isLast && isLastMultiline = markerMessagePrefixStart conf
                  | otherwise = multilineMarkerPrefixStart conf

            tell . annotate color . pretty $ prefix' : markerMessagePrefixEnd conf : " "
            case markerMessage marker of
              Nothing -> pure ()
              Just msg -> do
                let linePrefix
                      | isLast = prefix <> "   "
                      | otherwise = prefixWithBar (annotate color) <> space

                tell . annotate color . replaceLinesWith linePrefix id $ pretty msg

          showMultilineMarkerMessages [] = pure ()
          showMultilineMarkerMessages [m] =
            withSomeM (pure m) (`showMultilineMarkerMessage` True)
          showMultilineMarkerMessages (m : ms) = do
            withSomeM (pure m) (`showMultilineMarkerMessage` False)
            tell prefix
            showMultilineMarkerMessages ms

      tell $ prefixWithBar colorOfFirstMultilineMarker
      tell prefix
      showMultilineMarkerMessages multiline

showAllMarkersInLine ::
  Pretty msg =>
  Bool ->
  Bool ->
  (Doc AriadneAnnotation -> Doc AriadneAnnotation) ->
  Configuration ->
  Severity ->
  Int ->
  WidthTable ->
  [Some (Marker msg)] ->
  Layouter ()
showAllMarkersInLine _ _ _ _ _ _ _ [] = pure ()
showAllMarkersInLine hasMultilines inSpanOfMultiline colorMultilinePrefix conf sev leftLen widths ms = do
  let (_, maxMarkerColumn) = end . maximum $ (`withSome` markerPosition) <$> ms
      -- get the maximum end column, so that we know when to stop looking for other markers on the same line
      specialPrefix
        | inSpanOfMultiline = colorMultilinePrefix (pretty $ verticalRule conf : " ") <> space
        | hasMultilines = colorMultilinePrefix "  " <> space
        | otherwise = mempty

  tell $ hardline <> space
  tell $ dotPrefix leftLen conf <> space
  unless (null ms) do
    tell specialPrefix
    showMarkers 1 maxMarkerColumn
    showMessages specialPrefix ms maxMarkerColumn
  where
    widthAt i = 0 `fromMaybe` safeArrayIndex i widths
    widthsBetween start end =
      sum $ take (end - start) $ drop (start - 1) $ Array.elems widths

    showMarkers :: Int -> Int -> Layouter ()
    showMarkers n lineLen
      | n > lineLen = pure () -- reached the end of the line
      | otherwise = do
          let allMarkers = flip filter ms \mark ->
                let (isBlank, Range (_, bc) (_, ec) _) = withSome mark (isBlankMarker &&& markerPosition)
                 in not isBlank && n >= bc && n < ec

          -- only consider markers which span onto the current column
          case allMarkers of
            [] -> do
              tell $ fold (replicate (widthAt n) space)
              showMarkers (n + 1) lineLen
            marker : _ -> do
              let (Range (_, bc) _ _, (color, (msg, (start, middle)))) =
                    withSome marker $
                      markerPosition &&& markerColor sev &&& markerMessage &&& \case
                        AddCode {} -> (additionMarkerStart conf, additionMarkerMiddle conf)
                        RemoveCode {} -> (deletionMarkerStart conf, deletionMarkerMiddle conf)
                        Annotate {} -> (markerStart conf, markerMiddle conf)
                        _ -> (markerStart conf, markerMiddle conf)

              tell $
                annotate
                  color
                  if bc == n && isJust msg
                    then pretty start <> fold (replicate (widthAt n - 1) (pretty middle))
                    else fold (replicate (widthAt n) (pretty middle))

              showMarkers (n + 1) lineLen

    showMessages :: Pretty msg => Doc AriadneAnnotation -> [Some (Marker msg)] -> Int -> Layouter ()
    showMessages _ [] _ = pure () -- no more messages to show
    showMessages specialPrefix (msg : pipes) lineLen = do
      let (color, (message, Range b@(_, bc) _ _)) = withSome msg (markerColor sev &&& markerMessage &&& markerPosition)

          pipes' = ((`withSome` markerPosition) &&& id) <$> pipes

          filteredPipes = flip filter pipes' \(Range b1 _ _, m) ->
            let isBlank = withSome m isBlankMarker
             in not isBlank && b1 /= b
          -- record only the pipes corresponding to markers on different starting positions
          nubbedPipes = List.nubBy ((==) `on` (begin . fst)) filteredPipes
          -- and then remove all duplicates

          allColumns _ [] = (1, [])
          allColumns n ms@((Range (_, bc) _ _, col) : ms')
            | n == bc = bimap (+ 1) (col :) (allColumns (n + 1) ms')
            | n < bc = bimap (+ 1) (replicate (widthAt n) space <>) (allColumns (n + 1) ms)
            | otherwise = bimap (+ 1) (replicate (widthAt n) space <>) (allColumns (n + 1) ms')
          -- transform the list of remaining markers into a single document line

          hasSuccessor = length filteredPipes /= length pipes'

          lineStart pipes =
            let (n, docs) = allColumns 1 $ List.sortOn (snd . begin . fst) pipes
                numberOfSpaces = widthsBetween n bc
             in dotPrefix leftLen conf <+> specialPrefix <> fold docs <> pretty (replicate numberOfSpaces ' ')
          -- the start of the line contains the "dot"-prefix as well as all the pipes for all the still not rendered marker messages

          prefix = do
            let (pipesBefore, pipesAfter) = List.partition ((< bc) . snd . begin . fst) nubbedPipes
                -- split the list so that all pipes before can have `|`s but pipes after won't

                pipesBeforeRendered =
                  pipesBefore
                    <&> second \marker -> annotate (withSome marker (markerColor sev)) (pretty $ verticalRule conf)
                -- pre-render pipes which are before because they will be shown

                lastBeginPosition = snd . begin . fst <$> List.lastMay (List.sortOn (snd . begin . fst) pipesAfter)

                lineLen = case lastBeginPosition of
                  Nothing -> 0
                  Just col -> widthsBetween bc col

                currentPipe =
                  pretty
                    (if hasSuccessor then multilineMarkerPrefixStart conf else markerMessagePrefixStart conf)

                lineChar = pretty $ markerMessagePrefixMiddle conf
                pointChar = pretty $ markerMessagePrefixEnd conf

                bc' = bc + lineLen + 2
                -- consider pipes before, as well as pipes which came before the text rectangle bounds
                pipesBeforeMessageStart = List.filter ((< bc') . snd . begin . fst) pipesAfter
                -- also pre-render pipes which are before the message text bounds, because they will be shown if the message is on
                -- multiple lines
                pipesBeforeMessageRendered =
                  (pipesBefore <> pipesBeforeMessageStart)
                    <&> second \marker -> annotate (withSome marker (markerColor sev)) (pretty $ verticalRule conf)

            case message of
              Nothing -> pure ()
              Just msg -> do
                let linePrefix =
                      hardline
                        <+> lineStart pipesBeforeMessageRendered
                        <+> if List.null pipesBeforeMessageStart then "  " else " "

                tell $ lineStart pipesBeforeRendered
                tell . annotate color $ currentPipe <> fold (replicate lineLen lineChar) <> pointChar <> space
                tell . annotate color . replaceLinesWith linePrefix id $ pretty msg

      tell $ hardline <> space
      prefix
      showMessages specialPrefix pipes lineLen

-- | Extracts the color of a marker as a 'Doc' coloring function.
markerColor ::
  -- | Whether the marker is in an error context or not.
  --   This really makes a difference for a 'This' marker.
  Severity ->
  -- | The marker to extract the color from.
  Marker msg k ->
  -- | A function used to color a 'Doc'.
  AriadneAnnotation
markerColor sev (Primary {}) = PrimaryTint sev
markerColor _ (Secondary {}) = SecondaryTint
markerColor _ (Blank {}) = CodeTint -- we take the same color as the code, for it to be invisible
markerColor _ (AddCode {}) = AdditionTint
markerColor _ (RemoveCode {}) = RemovalTint
markerColor _ (Annotate {}) = SecondaryTint

isBlankMarker :: Marker msg k -> Bool
isBlankMarker (Blank {}) = True
isBlankMarker _ = False
{-# INLINE isBlankMarker #-}

prettySeverity :: Pretty msg => Severity -> Maybe msg -> Doc AriadneAnnotation
prettySeverity sev code =
  let sevdoc = case sev of
        Error -> "error"
        Warning -> "warning"
        Critical -> "critical"
      codedoc = case code of
        Nothing -> mempty
        Just code -> space <> pretty code
   in annotate (Header $ PrimaryTint sev) (lbracket <> sevdoc <> codedoc <> rbracket)

-- | Pretty prints all hints.
prettyAllHints :: Pretty msg => [Note msg] -> Int -> FileMap -> Configuration -> Severity -> Int -> Layouter ()
prettyAllHints [] _ _ _ _ _ = pure ()
prettyAllHints (h : hs) leftLen files conf sev tabSize = do
  {-
        A hint is composed of:
        (1)         : Hint: <hint message>
        (2)         +--> <file position>
        (3)  <line> | <source code possibly altered>
        (4)         : <hint marker>
  -}

  let marker = maybe (pure ()) showMarkerWithCode (noteMarker h)
      prefix = hardline <+> pipePrefix leftLen conf

  tell $ prefix <> space
  tell $ annotate NoteTint (notePrefix h <+> replaceLinesWith (prefix <+> "      ") id (pretty $ noteMessage h))
  marker

  case noteMarker h of
    Nothing
      | null hs -> tell $ hardline <> space
      | otherwise -> tell prefix
    Just (Annotate _ (Just _)) -> tell $ hardline <> space
    _ -> pure ()

  prettyAllHints hs leftLen files conf sev tabSize
  where
    notePrefix (Note _ _) = "Note:"
    notePrefix (Hint _ _) = "Help:"

    noteMessage (Note msg _) = msg
    noteMessage (Hint msg _) = msg

    noteMarker (Note _ m) = m
    noteMarker (Hint _ m) = m

    showMarkerWithCode = prettySubReport files conf sev tabSize leftLen False . pure . Some
