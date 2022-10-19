{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Error.Diagnose.Layout.Typescript (typescriptLayout) where

import Control.Arrow ((&&&))
import Control.Monad (forM, forM_, unless)
import Control.Monad.Trans.Writer (Writer, execWriter, tell)
import qualified Data.Array.IArray as Array
import Data.Foldable (fold)
import qualified Data.List as List
import Data.Some (Some (..), withSome)
import Error.Diagnose.Diagnostic (filesOf, reportsOf)
import Error.Diagnose.Layout (FileMap, Layout)
import Error.Diagnose.Layout.Typescript.Config (Configuration (..), configuration)
import Error.Diagnose.Layout.Typescript.Style (TypescriptAnnotation (..))
import Error.Diagnose.Position (SourceRange (..))
import Error.Diagnose.Pretty (Doc, Pretty, align, annotate, colon, hardline, indent, pretty, space, width)
import Error.Diagnose.Report (Marker (..), MarkerKind (..), Note (..), Report (..), Severity (..))
import Error.Diagnose.Utils (fetchLine, markerMessage, markerPosition, replaceLinesWith, safeArrayIndex)

-- | Pretty prints a 'Diagnostic' into a 'Doc'ument that can be output using 'hPutDoc'.
--   The generated 'Doc'ument should resemble GCC's error style.
--
--   Colors are put by default.
--   If you do not want these, just 'unAnnotate' the resulting document like so:
--
--   >>> let doc = unAnnotate (typescriptLayout withUnicode tabSize diagnostic)
typescriptLayout :: Layout msg TypescriptAnnotation
typescriptLayout withUnicode tabSize diag = fold . List.intersperse hardline $ go <$> reportsOf diag
  where
    go rep = execWriter $ prettyReport (filesOf diag) (configuration withUnicode) tabSize rep
{-# INLINE typescriptLayout #-}

--------------------------------------
------------- INTERNAL ---------------
--------------------------------------

type Layouter a = Writer (Doc TypescriptAnnotation) a

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

prettyReport' ::
  Pretty msg =>
  FileMap ->
  Configuration ->
  Int ->
  Severity ->
  Maybe msg ->
  msg ->
  [Marker msg 'MainMarker] ->
  [Note msg] ->
  Layouter ()
prettyReport' fileContent conf tabSize sev code message markers hints = do
  let (sortedMarkers, blanks) = List.partition (not . isBlankMarker) $ List.sortBy markerSorter markers
  -- sort the markers so that the 'This' markers are before the other ones

  other <- printHeader (List.uncons sortedMarkers)

  forM_ (other <> blanks) \m -> do
    let doc = execWriter $ prettyMarkerWithFile fileContent conf tabSize sev m
    tell $ indent 2 doc
    tell hardline
  unless (null blanks) do
    tell hardline

  let doc = execWriter $ prettyAllHints fileContent conf tabSize sev hints
  tell $ indent 2 doc
  where
    printHeader Nothing = do
      tell $ prettySeverity sev
      maybe (pure ()) (tell . mappend space . annotate ErrorCode . pretty) code
      tell $ colon <> space
      tell $ align (pretty message)
      tell hardline

      pure []
    printHeader (Just (m1@(Primary {}), ms)) = do
      let pos = markerPosition m1

      tell $
        annotate (Header $ PrimaryTint sev) case sev of
          Error -> "ERROR in"
          Warning -> "WARNING in"
          Critical -> "BUG in"
          <> space
      tell $ prettyPosition pos <> space
      tell $ "-" <> space
      _ <- printHeader Nothing
      let doc = execWriter $ prettyMarker fileContent conf tabSize sev m1
      tell $ indent 2 doc
      tell hardline

      pure ms
    printHeader (Just (m1, ms)) = do
      _ <- printHeader Nothing

      pure (m1 : ms)

-------------------------------------------------------------------------------------
----- INTERNAL STUFF ----------------------------------------------------------------
-------------------------------------------------------------------------------------

prettyMarkerWithFile :: Pretty msg => FileMap -> Configuration -> Int -> Severity -> Marker msg 'MainMarker -> Layouter ()
prettyMarkerWithFile fileContent conf tabSize sev m = do
  let pos = markerPosition m
      kind = case m of
        Primary {} -> prettySeverity sev
        Secondary {} -> annotate SecondaryTint "note"
        Blank {} -> annotate SecondaryTint "note"

  tell $ prettyPosition pos <> space
  tell $ "-" <> space
  tell kind
  tell colon
  tell hardline

  let doc = execWriter $ prettyMarker fileContent conf tabSize sev m
  tell $ indent 2 doc

prettyMarker :: Pretty msg => FileMap -> Configuration -> Int -> Severity -> Marker msg k -> Layouter ()
prettyMarker fileContent conf tabSize sev m1 = do
  let Range (lineB', _) (lineE', _) f = markerPosition m1

      (moreThanFour, lineNos)
        | lineE' - lineB' >= 4 = (True, [lineB', lineB' + 1, lineE' - 1, lineE'])
        | otherwise = (False, [lineB' .. lineE'])

      lines = flip map lineNos \line ->
        (line, fetchLine fileContent f line tabSize (NoLineTint, "<no line>") CodeTint (const CodeTint) [Some m1])

      maxLineNoSize = maximum $ (if moreThanFour then (3 :) else id) (length . show <$> lineNos)
      -- if there are more than four lines, then add the 3 dots of the ellipse to the width of
      -- the line number gutter

      lines'
        | moreThanFour = do
            let [l1, l2, l4, l5] = lines
            prettyLine False maxLineNoSize l1
            prettyLine False maxLineNoSize l2
            prettyLine True maxLineNoSize (-1, undefined)
            prettyLine False maxLineNoSize l4
            prettyLine False maxLineNoSize l5
        | otherwise = do
            forM_ lines (prettyLine False maxLineNoSize)

  lines'
  case markerMessage m1 of
    Nothing -> pure ()
    Just msg -> do
      tell $ indent (maxLineNoSize + 1) (align . annotate color $ pretty msg)
      tell hardline
  where
    totalWidth b e widths = maybe 0 sum (forM [b .. e] (`safeArrayIndex` widths))

    spaceBeforeUnderline b = totalWidth 1 (b - 1)
    underlineSize b e = totalWidth b (e - 1)
    -- various size computations

    color = markerColor sev m1

    tilde = pretty case m1 of
      AddCode {} -> additionChar conf
      RemoveCode {} -> removalChar conf
      _ -> markerChar conf

    underline colB colE widths = do
      tell $ pad (spaceBeforeUnderline colB widths) ' ' mempty
      tell $ annotate color (fold $ replicate (underlineSize colB colE widths) tilde)

    prettyLine hasEllipsis maxLineNoSize (lineNo, ~(widths, [m], line')) = do
      let (isBlank, Range (lineB, colB) (lineE, colE) _) = withSome m (isBlankMarker &&& markerPosition)

          lineNoSize = maxLineNoSize - if hasEllipsis then 3 else length (show lineNo)

          before = lineNoSize - after
          after = lineNoSize `div` 2

          number = fold (replicate before space) <> (if hasEllipsis then "..." else pretty lineNo) <> fold (replicate after space)
          emptyNumber = annotate (LineNumber sev) (fold $ replicate maxLineNoSize space)

          beginColumn = if lineB == lineNo then colB else 1
          endColumn = if lineE == lineNo then colE else snd (Array.bounds widths) + 1

      tell $ annotate (LineNumber sev) number <> space
      if hasEllipsis
        then tell hardline
        else do
          tell $ replaceLinesWith (hardline <> emptyNumber <> space) id line'
          unless isBlank do
            tell hardline
            tell $ emptyNumber <> space
            underline beginColumn endColumn widths
            tell hardline

-- | Pretty prints a position according to the format @<file>:<line>:<column>@
--   where @<line>@ and @<column>@ are only the beginning of the span.
prettyPosition :: SourceRange -> Doc TypescriptAnnotation
prettyPosition (Range (bc, bl) _ file) =
  annotate FileName (pretty file)
    <> colon
    <> annotate FilePosition (pretty bl)
    <> colon
    <> annotate FilePosition (pretty bc)

prettySeverity :: Severity -> Doc TypescriptAnnotation
prettySeverity Error = annotate (Header $ PrimaryTint Error) "error"
prettySeverity Warning = annotate (Header $ PrimaryTint Warning) "warning"
prettySeverity Critical = annotate (Header $ PrimaryTint Critical) "critical"

-- | Inserts a given number of character after a 'Doc'ument.
pad :: Int -> Char -> Doc ann -> Doc ann
pad n c d = width d \w -> pretty $ replicate (n - w) c

-- | Extracts the color of a marker as a 'Doc' coloring function.
markerColor ::
  -- | Whether the marker is in an error context or not.
  --   This really makes a difference for a 'This' marker.
  Severity ->
  -- | The marker to extract the color from.
  Marker msg k ->
  -- | A function used to color a 'Doc'.
  TypescriptAnnotation
markerColor sev (Primary {}) = PrimaryTint sev
markerColor _ (Secondary {}) = SecondaryTint
markerColor _ (Blank {}) = CodeTint -- we take the same color as the code, for it to be invisible
markerColor _ (AddCode {}) = AdditionTint
markerColor _ (RemoveCode {}) = RemovalTint
markerColor _ (Annotate {}) = SecondaryTint

markerSorter :: Marker msg k -> Marker msg k -> Ordering
-- k ~ 'MainMarker
markerSorter (Primary {}) (Primary {}) = EQ
markerSorter (Primary {}) _ = LT
markerSorter (Secondary {}) (Secondary {}) = EQ
markerSorter (Secondary {}) (Primary {}) = GT
markerSorter (Secondary {}) _ = LT
markerSorter (Blank {}) (Blank {}) = EQ
markerSorter (Blank {}) _ = GT
-- k ~ 'NoteMarker
markerSorter (AddCode {}) (AddCode {}) = EQ
markerSorter (AddCode {}) _ = LT
markerSorter (RemoveCode {}) (RemoveCode {}) = EQ
markerSorter (RemoveCode {}) (AddCode {}) = GT
markerSorter (RemoveCode {}) _ = LT
markerSorter (Annotate {}) (Annotate {}) = EQ
markerSorter (Annotate {}) _ = GT

isBlankMarker :: Marker msg k -> Bool
isBlankMarker (Blank {}) = True
isBlankMarker _ = False
{-# INLINE isBlankMarker #-}

-- | Pretty prints all hints.
prettyAllHints :: Pretty msg => FileMap -> Configuration -> Int -> Severity -> [Note msg] -> Layouter ()
prettyAllHints _ _ _ _ [] = pure ()
prettyAllHints files conf tabSize sev (h : hs) = do
  tell $ annotate NoteTint (notePrefix h)
  tell $ colon <> space
  tell $ align $ pretty (noteMessage h)
  tell hardline
  case noteNotes h of
    Nothing -> pure ()
    Just n -> do
      let doc = execWriter $ prettyMarker files conf tabSize sev n
      tell $ indent 2 doc
      tell hardline
  prettyAllHints files conf tabSize sev hs
  where
    noteNotes (Note _ ns) = ns
    noteNotes (Hint _ ns) = ns

    notePrefix (Note _ _) = "note"
    notePrefix (Hint _ _) = "help"

    noteMessage (Note msg _) = msg
    noteMessage (Hint msg _) = msg
