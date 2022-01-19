{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

{-# OPTIONS -Wno-name-shadowing #-}

{-|
Module      : Error.Diagnose.Report.Internal
Description : Internal workings for report definitions and pretty printing.
Copyright   : (c) Mesabloo, 2021
License     : BSD3
Stability   : experimental
Portability : Portable

/Warning/: The API of this module can break between two releases, therefore you should not rely on it.
           It is also highly undocumented.

           Please limit yourself to the "Error.Diagnose.Report" module, which exports some of the useful functions defined here.
-}
module Error.Diagnose.Report.Internal where

#ifdef USE_AESON
import Data.Aeson (ToJSON(..), object, (.=))
#endif
import Data.Bifunctor (first, second, bimap)
import Data.Default (def)
import Data.Foldable (fold)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.List as List
import qualified Data.List.Safe as List
import Data.Ord (Down(..))

import Error.Diagnose.Position

import Prettyprinter (Pretty(..), Doc, hardline, (<+>), align, space, annotate, width, colon)
import Prettyprinter.Internal (Doc(..))
import Prettyprinter.Render.Terminal (color, colorDull, Color(..), bold, AnsiStyle)


-- | The type of diagnostic reports with abstract message type.
data Report msg
  = Report
        Bool                            -- ^ Is the report a warning or an error?
        msg                             -- ^ The message associated with the error.
        [(Position, Marker msg)]        -- ^ A map associating positions with marker to show under the source code.
        [msg]                           -- ^ A list of hints to add at the end of the report.

instance Semigroup msg => Semigroup (Report msg) where
  Report isError1 msg1 pos1 hints1 <> Report isError2 msg2 pos2 hints2 =
    Report (isError1 || isError2) (msg1 <> msg2) (pos1 <> pos2) (hints1 <> hints2)

instance Monoid msg => Monoid (Report msg) where
  mempty = Report False mempty mempty mempty

#ifdef USE_AESON
instance ToJSON msg => ToJSON (Report msg) where
  toJSON (Report isError msg markers hints) =
    object [ "kind" .= (if isError then "error" else "warning" :: String)
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
  This _ == This _   = True
  Where _ == Where _ = True
  Maybe _ == Maybe _ = True
  _ == _             = False
  {-# INLINABLE (==) #-}

instance Ord (Marker msg) where
  This _ < _       = False
  Where _ < This _ = True
  Where _ < _      = False
  Maybe _ < _      = True
  {-# INLINABLE (<) #-}

  m1 <= m2         = m1 < m2 || m1 == m2
  {-# INLINABLE (<=) #-}


-- | Constructs a warning or an error report.
warn, err :: msg                             -- ^ The report message, shown at the very top.
          -> [(Position, Marker msg)]        -- ^ A list associating positions with markers.
          -> [msg]                           -- ^ A possibly mempty list of hints to add at the end of the report.
          -> Report msg
warn = Report False
{-# INLINE warn #-}
err = Report True
{-# INLINE err #-}

-- | Pretty prints a report to a 'Doc' handling colors.
prettyReport :: Pretty msg
             => HashMap FilePath [String]       -- ^ The content of the file the reports are for
             -> Bool                            -- ^ Should we print paths in unicode?
             -> Report msg                      -- ^ The whole report to output
             -> Doc AnsiStyle
prettyReport fileContent withUnicode (Report isError message markers hints) =
  let sortedMarkers = List.sortOn (fst . begin . fst) markers
      -- sort the markers so that the first lines of the reports are the first lines of the file

      (markersPerLine, multilineMarkers) = splitMarkersPerLine sortedMarkers
      -- split the list on whether markers are multiline or not

      sortedMarkersPerLine = {- second (List.sortOn (first $ snd . begin)) <$> -} List.sortOn fst (HashMap.toList markersPerLine)

      reportFile = maybe (pretty @Position def) (pretty . fst) $ List.safeHead (filter (isThisMarker . snd) sortedMarkers)
      -- the reported file is the file of the first 'This' marker (only one must be present)

      header = annotate bold if isError then annotate (color Red) "[error]" else annotate (color Yellow) "[warning]"

      maxLineNumberLength = maybe 3 (max 3 . length . show . fst . end . fst) $ List.safeLast sortedMarkers
      -- if there are no markers, then default to 3, else get the maximum between 3 and the length of the last marker

      allLineNumbers = List.sort $ List.nub $ (fst <$> sortedMarkersPerLine) <> (multilineMarkers >>= \ (Position (bl, _) (el, _) _, _) -> [bl..el])
{-
        A report is of the form:
        (1)    [error|warning]: <message>
        (2)           +-> <file>
        (3)           :
        (4)    <line> | <line of code>
                      : <marker lines>
                      : <marker messages>
        (5)           :
                      : <hints>
        (6)    -------+
-}
  in  {- (1) -} header <> colon <+> align (pretty message) <> hardline
  <+> {- (2) -} pad maxLineNumberLength ' ' mempty <+> annotate (bold <> color Black) (if withUnicode then "╭─▶" else "+->") <+> annotate (bold <> colorDull Green) reportFile <> hardline
  <+> {- (3) -} pipePrefix maxLineNumberLength withUnicode
  <>  {- (4) -} prettyAllLines fileContent withUnicode isError maxLineNumberLength sortedMarkersPerLine multilineMarkers allLineNumbers
  <>  {- (5) -} (if null hints || null markers then mempty else hardline <+> dotPrefix maxLineNumberLength withUnicode) <> prettyAllHints hints maxLineNumberLength withUnicode <> hardline
  <>  {- (6) -} annotate (bold <> color Black) (pad (maxLineNumberLength + 2) (if withUnicode then '─' else '-') mempty <> if withUnicode then "╯" else "+") <> hardline
  where
    isThisMarker (This _) = True
    isThisMarker _        = False


-------------------------------------------------------------------------------------
----- INTERNAL STUFF ----------------------------------------------------------------
-------------------------------------------------------------------------------------

-- | Inserts a given number of character after a 'Doc'ument.
pad :: Int -> Char -> Doc ann -> Doc ann
pad n c d = width d \ w -> pretty $ replicate (n - w) c

-- | Creates a "dot"-prefix for a report line where there is no code.
--
--   Pretty printing yields those results:
--
--   [with unicode] "@␣␣␣␣␣•␣@"
--   [without unicode] "@␣␣␣␣␣:␣@"
dotPrefix :: Int        -- ^ The length of the left space before the bullet.
          -> Bool       -- ^ Whether to print with unicode characters or not.
          -> Doc AnsiStyle
dotPrefix leftLen withUnicode = pad leftLen ' ' mempty <+> annotate (bold <> color Black) (if withUnicode then "•" else ":")
{-# INLINE dotPrefix #-}

-- | Creates a "pipe"-prefix for a report line where there is no code.
--
--   Pretty printing yields those results:
--
--   [with unicode] "@␣␣␣␣␣│␣@"
--   [without unicode] "@␣␣␣␣␣|␣@"
pipePrefix :: Int       -- ^ The length of the left space before the pipe.
           -> Bool      -- ^ Whether to print with unicode characters or not.
           -> Doc AnsiStyle
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
linePrefix :: Int       -- ^ The length of the amount of space to span before the vertical bar.
           -> Int       -- ^ The line number to show.
           -> Bool      -- ^ Whether to use unicode characters or not.
           -> Doc AnsiStyle
linePrefix leftLen lineNo withUnicode =
  let lineNoLen = length (show lineNo)
  in annotate (bold <> color Black) $ mempty <+> pad (leftLen - lineNoLen) ' ' mempty <> pretty lineNo <+> if withUnicode then "│" else "|"
{-# INLINE linePrefix #-}

-- |
splitMarkersPerLine :: [(Position, Marker msg)] -> (HashMap Int [(Position, Marker msg)], [(Position, Marker msg)])
splitMarkersPerLine []                         = (mempty, mempty)
splitMarkersPerLine (m@(Position{..}, _) : ms) =
  let (bl, _) = begin
      (el, _) = end
  in (if bl == el then first (HashMap.insertWith (<>) bl [m]) else second (m :))
        (splitMarkersPerLine ms)

-- |
prettyAllLines :: Pretty msg
               => HashMap FilePath [String]
               -> Bool
               -> Bool
               -> Int
               -> [(Int, [(Position, Marker msg)])]
               -> [(Position, Marker msg)]
               -> [Int]
               -> Doc AnsiStyle
prettyAllLines _ _ _ _ _ [] []                                                = mempty
prettyAllLines _ withUnicode isError leftLen _ multiline []                   =
  let colorOfLastMultilineMarker = maybe mempty (markerColor isError . snd) (List.safeLast multiline)
      -- take the color of the last multiline marker in case we need to add additional bars

      prefix = hardline <+> dotPrefix leftLen withUnicode <> space
      prefixWithBar color = prefix <> annotate color (if withUnicode then "│ " else "| ")

      showMultilineMarkerMessage (_, marker) isLast = annotate (markerColor isError marker) $ (if isLast
                                                                                    then if withUnicode then "╰╸ " else "`- "
                                                                                    else if withUnicode then "├╸ " else "|- ")
                                            <> replaceLinesWith (if isLast then prefix <> "   " else prefixWithBar (markerColor isError marker) <> space) (pretty $ markerMessage marker)

      showMultilineMarkerMessages []       = []
      showMultilineMarkerMessages [m]      = [showMultilineMarkerMessage m True]
      showMultilineMarkerMessages (m : ms) = showMultilineMarkerMessage m False : showMultilineMarkerMessages ms

  in  prefixWithBar colorOfLastMultilineMarker <> prefix <> fold (List.intersperse prefix $ showMultilineMarkerMessages $ reverse multiline)
prettyAllLines files withUnicode isError leftLen inline multiline (line : ls) =
{-
    A line of code is composed of:
    (1)     <line> | <source code>
    (2)            : <markers>
    (3)            : <marker messages>


    Multline markers may also take additional space (2 characters) on the right of the bar
-}
  let allInlineMarkersInLine = snd =<< filter ((==) line . fst) inline

      allMultilineMarkersInLine = flip filter multiline \ (Position (bl, _) (el, _) _, _) -> bl == line || el == line

      allMultilineMarkersSpanningLine = flip filter multiline \ (Position (bl, _) (el, _) _, _) -> bl < line && el > line

      inSpanOfMultiline = flip any multiline \ (Position (bl, _) (el, _) _, _) -> bl <= line && el >= line

      colorOfFirstMultilineMarker = maybe id (annotate . markerColor isError . snd) (List.safeHead $ allMultilineMarkersInLine <> allMultilineMarkersSpanningLine)
      -- take the first multiline marker to color the entire line, if there is one

      !additionalPrefix = case allMultilineMarkersInLine of
        []                                           ->
          if not $ null multiline
          then if not $ null allMultilineMarkersSpanningLine
               then colorOfFirstMultilineMarker if withUnicode then "│  " else "|  "
               else "   "
          else mempty
        (p@(Position _ (el, _) _), marker) : _ ->
          let hasPredecessor = el == line || maybe False ((/=) p . fst . fst) (List.safeUncons multiline)
          in  colorOfFirstMultilineMarker (if | hasPredecessor && withUnicode -> "├"
                                              | hasPredecessor                -> "|"
                                              | withUnicode                   -> "╭"
                                              | otherwise                     -> "+")
           <> annotate (markerColor isError marker) (if withUnicode then "┤" else ">")
           <> space

      allMarkersInLine = {- List.sortOn fst $ -} allInlineMarkersInLine <> allMultilineMarkersInLine
  in  hardline
   <> {- (1) -} linePrefix leftLen line withUnicode <+> additionalPrefix <> getLine_ files (allMarkersInLine <> allMultilineMarkersSpanningLine) line isError
   <> {- (2) -} showAllMarkersInLine (not $ null multiline) inSpanOfMultiline colorOfFirstMultilineMarker withUnicode isError leftLen allInlineMarkersInLine
   <> {- (3) -} prettyAllLines files withUnicode isError leftLen inline multiline ls

-- |
getLine_ :: HashMap FilePath [String] -> [(Position, Marker msg)] -> Int -> Bool -> Doc AnsiStyle
getLine_ files markers line isError = case List.safeIndex (line - 1) =<< (HashMap.!?) files . file . fst =<< List.safeHead markers of
  Nothing   -> annotate (bold <> colorDull Magenta) "<no line>"
  Just code -> fold $ indexed code <&> \ (n, c) ->
    let colorizingMarkers = flip filter markers
          \ (Position (bl, bc) (el, ec) _, _) ->
             if bl == el
             then n >= bc && n < ec
             else (bl == line && n >= bc) || (el == line && n < ec)

    in  maybe id ((\ m -> annotate (bold <> markerColor isError m)) . snd) (List.safeHead colorizingMarkers) (pretty c)
  where
    indexed :: [a] -> [(Int, a)]
    indexed = goIndexed 1

    goIndexed :: Int -> [a] -> [(Int, a)]
    goIndexed _ []       = []
    goIndexed n (x : xs) = (n, x) : goIndexed (n + 1) xs

-- |
showAllMarkersInLine :: Pretty msg => Bool -> Bool -> (Doc AnsiStyle -> Doc AnsiStyle) -> Bool -> Bool -> Int -> [(Position, Marker msg)] -> Doc AnsiStyle
showAllMarkersInLine _ _ _ _ _ _ []                                                      = mempty
showAllMarkersInLine hasMultilines inSpanOfMultiline colorMultilinePrefix withUnicode isError leftLen ms =
  let maxMarkerColumn = snd $ end $ fst $ List.last $ List.sortOn (snd . end . fst) ms
      specialPrefix = if inSpanOfMultiline
                      then colorMultilinePrefix (if withUnicode then "│ " else "| ") <> space
                      else if hasMultilines
                      then colorMultilinePrefix "  " <> space
                      else mempty
      -- get the maximum end column, so that we know when to stop looking for other markers on the same line
  in  hardline <+> dotPrefix leftLen withUnicode <+> (if List.null ms then mempty else specialPrefix <> showMarkers 1 maxMarkerColumn <> showMessages specialPrefix ms maxMarkerColumn)
  where
    showMarkers n lineLen
      | n > lineLen  = mempty -- reached the end of the line
      | otherwise    =
        let allMarkers = flip filter ms \ (Position (_, bc) (_, ec) _, _) -> n >= bc && n < ec
            -- only consider markers which span onto the current column
        in  case allMarkers of
              []                      -> space <> showMarkers (n + 1) lineLen
              (Position{..},marker):_ ->
                if snd begin == n
                then annotate (markerColor isError marker) (if withUnicode then "┬" else "^") <> showMarkers (n + 1) lineLen
                else annotate (markerColor isError marker) (if withUnicode then "─" else "-") <> showMarkers (n + 1) lineLen
                -- if the marker just started on this column, output a caret, else output a dash

    showMessages specialPrefix ms lineLen = case List.safeUncons ms of
      Nothing                                     -> mempty -- no more messages to show
      Just ((Position b@(_, bc) _ _, msg), pipes) ->
        let filteredPipes = filter ((/= b) . begin . fst) pipes
            -- record only the pipes corresponding to markers on different starting positions
            nubbedPipes = List.nubBy ((==) `on` (begin . fst)) filteredPipes
            -- and then remove all duplicates

            allColumns _ []                                     = (1, [])
            allColumns n ms@((Position (_, bc) _ _, col) : ms')
              | n == bc                                         = bimap (+ 1) (col :) (allColumns (n + 1) ms')
              | n < bc                                          = bimap (+ 1) (space :) (allColumns (n + 1) ms)
              | otherwise                                       = bimap (+ 1) (space :) (allColumns (n + 1) ms')
              -- transform the list of remaining markers into a single document line

            hasSuccessor = length filteredPipes /= length pipes

            lineStart pipes =
              let (n, docs) = allColumns 1 $ List.sortOn (snd . begin . fst) pipes
              in  dotPrefix leftLen withUnicode <+> specialPrefix <> fold docs <> pretty (replicate (bc - n) ' ')
              -- the start of the line contains the "dot"-prefix as well as all the pipes for all the still not rendered marker messages

            prefix =
              let (pipesBefore, pipesAfter) = List.partition ((< bc) . snd . begin . fst) nubbedPipes
                  -- split the list so that all pipes before can have `|`s but pipes after won't

                  pipesBeforeRendered = pipesBefore <&> second \ marker -> annotate (markerColor isError marker) (if withUnicode then "│" else "|")
                  -- pre-render pipes which are before because they will be shown

                  lastBeginPosition = snd . begin . fst <$> List.safeLast (List.sortOn (Down . snd . begin . fst) pipesAfter)

                  lineLen = case lastBeginPosition of
                    Nothing  -> 0
                    Just col -> col - bc

                  currentPipe = if | withUnicode && hasSuccessor -> "├"
                                   | withUnicode                 -> "╰"
                                   | hasSuccessor                -> "|"
                                   | otherwise                   -> "`"

                  lineChar = if withUnicode then '─' else '-'
                  pointChar = if withUnicode then "╸" else "-"

              in lineStart pipesBeforeRendered
                   <>  annotate (markerColor isError msg) (currentPipe <> pretty (replicate lineLen lineChar) <> pointChar)
                   <+> annotate (markerColor isError msg) (replaceLinesWith (hardline <+> lineStart pipesBeforeRendered <+> "  ") $ pretty $ markerMessage msg)

        in  hardline <+> prefix <> showMessages specialPrefix pipes lineLen

-- WARN: uses the internal of the library
--
--       DO NOT use a wildcard here, in case the internal API exposes one more constructor
-- |
replaceLinesWith :: Doc ann -> Doc ann -> Doc ann
replaceLinesWith repl Line                   = repl
replaceLinesWith _ Fail                      = Fail
replaceLinesWith _ Empty                     = Empty
replaceLinesWith _ (Char c)                  = Char c
replaceLinesWith _ (Text n s)                = Text n s
replaceLinesWith repl (FlatAlt f d)          = FlatAlt (replaceLinesWith repl f) (replaceLinesWith repl d)
replaceLinesWith repl (Cat c d)              = Cat (replaceLinesWith repl c) (replaceLinesWith repl d)
replaceLinesWith repl (Nest n d)             = Nest n (replaceLinesWith repl d)
replaceLinesWith repl (Union c d)            = Union (replaceLinesWith repl c) (replaceLinesWith repl d)
replaceLinesWith repl (Column f)             = Column (replaceLinesWith repl . f)
replaceLinesWith repl (Nesting f)            = Nesting (replaceLinesWith repl . f)
replaceLinesWith repl (Annotated ann doc)    = Annotated ann (replaceLinesWith repl doc)
replaceLinesWith repl (WithPageWidth f)      = WithPageWidth (replaceLinesWith repl . f)

-- | Extracts the color of a marker as a 'Doc' coloring function.
markerColor :: Bool             -- ^ Whether the marker is in an error context or not.
                                --   This really makes a difference for a 'This' marker.
            -> Marker msg       -- ^ The marker to extract the color from.
            -> AnsiStyle        -- ^ A function used to color a 'Doc'.
markerColor isError (This _) = if isError then color Red else color Yellow
markerColor _ (Where _)      = colorDull Blue
markerColor _ (Maybe _)      = color Magenta
{-# INLINE markerColor #-}

-- | Retrieves the message held by a marker.
markerMessage :: Marker msg -> msg
markerMessage (This m)  = m
markerMessage (Where m) = m
markerMessage (Maybe m) = m
{-# INLINE markerMessage #-}

-- | Pretty prints all hints.
prettyAllHints :: Pretty msg => [msg] -> Int -> Bool -> Doc AnsiStyle
prettyAllHints [] _ _                       = mempty
prettyAllHints (h : hs) leftLen withUnicode =
{-
      A hint is composed of:
      (1)         : Hint: <hint message>
-}
  let prefix = hardline <+> pipePrefix leftLen withUnicode
  in  prefix <+> annotate (color Cyan) (annotate bold "Hint:" <+> replaceLinesWith (prefix <+> "      ") (pretty h))
   <> prettyAllHints hs leftLen withUnicode
