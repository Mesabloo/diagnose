{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module Text.Diagnose.Report
( Report, Marker(..), Files, Kind, Hint
, reportError, reportWarning
, hint

, prettyReport
) where

import Text.Diagnose.Position
import Text.Diagnose.Format
import Text.PrettyPrint.ANSI.Leijen
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as List
import Prelude hiding ((<$>))
import Data.Functor ((<&>))
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (fromJust, maybeToList)

type Files s a = Map FilePath [s a]
type Markers m = Map Position (NonEmpty (Marker m))

-- | A report holds a 'Kind' of report and a message along with the useful 'Marker's and 'Hint'.
--
--   It basically holds either an error or a warning along with additional context such as code.
data Report m
  = Report Kind m (Markers m) [Hint m]

-- | The kind of a 'Report', either an error or a warning.
data Kind
  = Error
  | Warning

-- | A simple polymorphic hint holder
data Hint m
  = Hint m

-- | A polymorphic marker, parameterized on the message type.
--
--   A marker is either:
data Marker m
  = This m   -- ^ * a "this" marker (@^^^^^ \<message\>@) used to highlight where the error/warning is located at
  | Where m  -- ^ * a "where" marker (@----- \<message\>@) used to provide some useful information in the context
  | Maybe m  -- ^ * a "maybe" marker (@~~~~~ \<message\>@) used to provide ideas of potential fixes
  | Empty    -- ^ * an "empty" marker used to show a line in the error/warning without adding any sort of marker on it

-- | Creates a new report.
reportError, reportWarning :: m -> [(Position, Marker m)] -> [Hint m] -> Report m
reportError = newReport Error
reportWarning = newReport Warning

-- | Internal creation of a new report.
newReport :: Kind -> m -> [(Position, Marker m)] -> [Hint m] -> Report m
newReport sev msg markers hints = Report sev msg markMap hints
  where markMap               = foldl createMap mempty markers
        -- | Extends a 'Map' with a marker at a given position.
        --
        --   If the position is already in the 'Map', the marker is simply added to the list of markers
        --   else it is added as a non-empty list directly in the 'Map'.
        createMap m (p, mark) = Map.insertWith (flip (<>)) p (mark List.:| []) m

-- | A simple alias on the constructor of 'Hint', used to avoid exporting the constructor.
hint :: m -> Hint m
hint = Hint



-- | Prettifies a report, when given the files it may want to used.
prettyReport :: (Foldable s, PrettyText (s a), PrettyText m) => Files s a -> Report m -> Doc
prettyReport files (Report kind msg markers hints) =
  let (color, margin, sev) = prettyKind kind
  in color (bold sev) <> colon <+> prettyText msg <$>
     mconcat (replicate (margin - 2) space) <> text "In" <> colon <+>
     prettyCodeWithMarkers files markers color <$> line <>
     prettyHints hints

-- | Prettifies the kind of a report.
prettyKind :: Kind
           -> (Doc -> Doc, Int, Doc) -- ^ Returns the color for "this" markers, the offset for the "In: <file>" part and the label of the report
prettyKind Error   = (red, 7, brackets $ text "error")
prettyKind Warning = (yellow, 9, brackets $ text "warning")

-- | Prettifies the code along with the useful markers.
prettyCodeWithMarkers :: (Foldable s, PrettyText m, PrettyText (s a))
                      => Files s a    -- ^ The potential input files to use to show the code
                      -> Markers m    -- ^ The markers to show
                      -> (Doc -> Doc) -- ^ The color for "this" markers
                      -> Doc
prettyCodeWithMarkers files markers color =
  let sortedMarkers = sortBy (compare `on` fst) (Map.toList markers)
  in case sortedMarkers of
    []                                   -> green (text "???")
    (Position{beginning=begin, ..}, _):_ ->
      let (bLine, bCol)  = begin
          ((p, _):_)     = reverse sortedMarkers
          maxLineMarkLen = length (show (fst (beginning p)))

          showLine l     =
            space <> text (replicate (maxLineMarkLen - length (show l)) ' ') <> integer l <> text "|"

          fileContent    = fromJust (Map.lookup file files)

          showMarkers    = sortedMarkers <&> uncurry \ Position{..} markers ->
            let (bLine, bCol)  = beginning
                (eLine, eCol)  = end

                code           = fileContent !! fromIntegral (bLine - 1)

                underlineLen   = fromIntegral $ (if eLine == bLine then eCol else fromIntegral (length code)) - bCol

                marker m       = prettyMarker underlineLen m color magenta dullgreen
                renderMarker m =
                  marker m <&> \ x -> mconcat (replicate (maxLineMarkLen + 2 + fromIntegral bCol) space) <> x

                renderedMarkers = List.toList markers >>= maybeToList . renderMarker
            in white $ bold (showLine bLine) <+> prettyText code <>
               mconcat (applyIfNotNull (line :) $ punctuate line renderedMarkers)
      in green (text file) <$>
         empty <$>
         mconcat (punctuate line showMarkers)

-- | Prettifies a list of 'Hint's into a single 'Doc'ument. All 'Hint's are prettified and concatenated with a 'line' in between.
prettyHints :: (PrettyText m) => [Hint m] -> Doc
prettyHints [] = line
prettyHints hs = blue (fillSep $ punctuate line (fmap render hs)) <> line
  where render (Hint msg) = smartPretty msg

-- | Prettifies a marker.
prettyMarker :: (PrettyText m)
             => Int          -- ^ The length of the marker
             -> Marker m     -- ^ The marker to show
             -> (Doc -> Doc) -- ^ The color if a "this" marker
             -> (Doc -> Doc) -- ^ The color for a "where" marker
             -> (Doc -> Doc) -- ^ The color for a "maybe" marker
             -> Maybe Doc    -- ^ 'Nothing' if it is the 'Empty' marker
prettyMarker underlineLen marker colorThis colorWhere colorMaybe = case marker of
  Empty     -> Nothing
  This msg  -> Just (colorThis $ under '^' <+> align (smartPretty msg))
  Where msg -> Just (colorWhere $ under '-' <+> align (smartPretty msg))
  Maybe msg -> Just (colorMaybe $ under '~' <+> align (smartPretty msg))
 where under   = text . replicate underlineLen

-- | A smarter pretty to keep long texts in between the bounds and correctly align them.
smartPretty :: (PrettyText d) => d -> Doc
smartPretty = fillSep . fmap text . words . show . prettyText


-- | Applies a function to the list if it isn't '[]', else returns it.
applyIfNotNull :: ([a] -> [a]) -> [a] -> [a]
applyIfNotNull _ [] = []
applyIfNotNull f l = f l
