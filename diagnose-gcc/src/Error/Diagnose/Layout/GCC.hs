{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Error.Diagnose.Layout.GCC (gccLayout) where

import Control.Monad (forM, forM_, unless, when)
import Control.Monad.Trans.Writer (Writer, execWriter, tell)
import qualified Data.Array.IArray as Array
import Data.Foldable (fold)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Some (Some (..), foldSome, withSomeM)
import Error.Diagnose.Diagnostic (filesOf, reportsOf)
import Error.Diagnose.Layout (FileMap, Layout)
import Error.Diagnose.Layout.GCC.Config (Configuration (..), configuration)
import Error.Diagnose.Layout.GCC.Style (GccAnnotation (..))
import Error.Diagnose.Position (SourceRange (..))
import Error.Diagnose.Pretty (Doc, Pretty, align, annotate, brackets, colon, hardline, indent, pretty, space, width)
import Error.Diagnose.Report (Marker (..), MarkerKind (..), Note (..), Report (..), Severity (..))
import Error.Diagnose.Utils (fetchLine, markerMessage, markerPosition, safeArrayIndex)

-- | Pretty prints a 'Diagnostic' into a 'Doc'ument that can be output using 'hPutDoc'.
--   The generated 'Doc'ument should resemble GCC's error style.
--
--   Colors are put by default.
--   If you do not want these, just 'unAnnotate' the resulting document like so:
--
--   >>> let doc = unAnnotate (gccLayout withUnicode tabSize diagnostic)
gccLayout :: Layout msg GccAnnotation
gccLayout withUnicode tabSize diag = foldMap go $ reportsOf diag
  where
    go rep = execWriter $ prettyReport (filesOf diag) (configuration withUnicode) tabSize rep
{-# INLINE gccLayout #-}

type Layouter a = Writer (Doc GccAnnotation) a

--------------------------------------
------------- INTERNAL ---------------
--------------------------------------

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
prettyReport fileContent withUnicode tabSize (Report sev code message markers hints) =
  prettyReport' fileContent withUnicode tabSize sev code message markers hints

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
  -- sort the markers so that the 'Primary' markers are before the other ones

  {-
          A report is of the form:
          (1)    <file>: [error|warning] <message> [<error-code>]
          (2)           <marker 1 message>
                   <line of code>
                   <marker 1 underline>
          (3)    <file>: [error|warning|note|fix]: <marker N message>
                   <line of code>
                   <marker N underline>
          (4)    [note|help]: <message>
  -}

  {- (1), (2) -}
  other <- printHeader (List.uncons sortedMarkers)

  {- (3) -}
  forM_ (other <> blanks) $ prettyMarkerWithFile fileContent conf tabSize sev True
  when (null sortedMarkers) do
    tell hardline

  {- (4) -}
  prettyAllHints fileContent conf tabSize sev hints
  tell hardline
  where
    printHeader Nothing = do
      tell $ prettySeverity sev
      tell space
      tell $ align (pretty message)

      pure []
    printHeader (Just (m1@Primary {}, ms)) = do
      let pos = markerPosition m1

      tell $ annotate FilePosition (prettyPosition pos)
      tell $ colon <> space
      tell $ prettySeverity sev <> space
      tell $ align (pretty message) <> space
      maybe (pure ()) (tell . annotate (PrimaryTint sev) . brackets . pretty) code
      tell hardline
      prettyMarker fileContent conf tabSize sev False m1

      pure ms
    printHeader (Just (m1, ms)) = do
      tell $ prettySeverity sev <> space
      tell $ align (pretty message)
      tell hardline

      pure (m1 : ms)

-------------------------------------------------------------------------------------
----- INTERNAL STUFF ----------------------------------------------------------------
-------------------------------------------------------------------------------------

prettyMarkerWithFile :: Pretty msg => FileMap -> Configuration -> Int -> Severity -> Bool -> Marker msg 'MainMarker -> Layouter ()
prettyMarkerWithFile fileContent conf tabSize sev putMessageOnLine m = do
  let kind = case m of
        Blank {} -> annotate SecondaryTint "note:"
        Primary {} -> annotate (PrimaryTint sev) "error:"
        Secondary {} -> annotate SecondaryTint "note:"

      pos = markerPosition m

  tell $ annotate FilePosition (prettyPosition pos)
  tell $ colon <> space
  tell $ kind <> space
  prettyMarker fileContent conf tabSize sev putMessageOnLine m

prettyMarker :: Pretty msg => FileMap -> Configuration -> Int -> Severity -> Bool -> Marker msg k -> Layouter ()
prettyMarker files conf tabSize sev putMessageOnLine m = do
  let Range (lineB, _) _ f = markerPosition m

  let (widths, [mark], line) = fetchLine files f lineB tabSize (NoLineTint, "<no line>") CodeTint (foldSome $ markerColor sev) [Some m]

      isAdditionMarker = case m of AddCode {} -> True; _ -> False
      isRemovalMarker = case m of RemoveCode _ -> True; _ -> False

      (single, start, middle, end)
        | isAdditionMarker = (singleAddition, startAddition, middleAddition, endAddition)
        | isRemovalMarker = (singleRemoval, startRemoval, middleRemoval, endRemoval)
        | otherwise = (singleMarker, startMarker, middleMarker, endMarker)

      caret nth isSingle =
        let width = fromMaybe 1 $ safeArrayIndex nth widths
         in if
                | width == 0 -> mempty
                | isSingle && width == 1 -> pretty (single conf)
                | isSingle -> pretty (start conf) <> fold (replicate (width - 2) tilde) <> end' nth
                | otherwise -> pretty (start conf) <> fold (replicate (width - 1) tilde)

      tilde = pretty $ middle conf

      end' nth =
        let width = fromMaybe 1 $ safeArrayIndex nth widths
         in if
                | width == 0 -> mempty
                | width == 1 -> pretty $ end conf
                | otherwise -> fold (replicate (width - 1) tilde) <> pretty (end conf)
      -- these two functions are there just so that we don't repeat ourselves later on

      totalWidth b e = maybe 0 sum (forM [b .. e] (`safeArrayIndex` widths))

  withSomeM (pure mark) \mark -> do
    let Range (lineB, colB) (lineE, colE) _ = markerPosition mark

        color = markerColor sev mark

        spaceBeforeUnderline = totalWidth 1 (colB - 1)
        underlineSize b e = totalWidth b (e - 1)
        -- various size computations

        underline = do
          tell . indent 2 $ pad spaceBeforeUnderline ' ' mempty
          tell $
            annotate
              color
              if
                  | colE == colB + 1 && lineB == lineE -> caret colB True
                  | lineE /= lineB ->
                      fold $
                        [caret colB False]
                          <> replicate (underlineSize colB (snd (Array.bounds widths))) tilde
                          <> [pretty (ellipsis conf)]
                  | otherwise ->
                      fold $
                        [caret colB False]
                          <> replicate (underlineSize (colB + 1) (colE - 1)) tilde
                          <> [end' (colE - 1)]

    case markerMessage mark of
      Just msg -> do
        unless putMessageOnLine do
          tell "      "
        tell . align $ pretty msg
        tell hardline
      Nothing -> when putMessageOnLine do
        tell hardline
    tell $ indent 2 line
    unless (isBlankMarker mark) do
      tell hardline
      underline
    tell hardline

-- | Pretty prints a position according to the format @<file>:<line>:<column>@
--   where @<line>@ and @<column>@ are only the beginning of the span.
prettyPosition :: SourceRange -> Doc GccAnnotation
prettyPosition (Range (bc, bl) _ file) = pretty file <> colon <> pretty bl <> colon <> pretty bc

prettySeverity :: Severity -> Doc GccAnnotation
prettySeverity sev = annotate (Header $ PrimaryTint sev) case sev of
  Error -> "error:"
  Warning -> "warning:"
  Critical -> "bug:"

-- | Inserts a given number of characters after a 'Doc'ument.
pad :: Int -> Char -> Doc ann -> Doc ann
pad n c d = width d \w -> pretty $ replicate (n - w) c

markerColor :: Severity -> Marker msg k -> GccAnnotation
markerColor sev (Primary _ _) = PrimaryTint sev
markerColor _ (Secondary _ _) = SecondaryTint
markerColor _ (Blank _) = CodeTint
markerColor _ (AddCode {}) = AdditionTint
markerColor _ (RemoveCode _) = RemovalTint
markerColor _ (Annotate _ _) = SecondaryTint

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
  tell $ annotate NoteTint (notePrefix h) <> space
  tell $ align (pretty $ noteMessage h)
  case noteNotes h of
    Nothing -> do
      tell hardline
    Just n -> do
      tell hardline
      prettyMarker files conf tabSize sev False n
  prettyAllHints files conf tabSize sev hs
  where
    noteNotes (Note _ ns) = ns
    noteNotes (Hint _ ns) = ns

    notePrefix (Note _ _) = "note:"
    notePrefix (Hint _ _) = "help:"

    noteMessage (Note msg _) = msg
    noteMessage (Hint msg _) = msg
