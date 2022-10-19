{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- |
-- Module: Error.Diagnose.Utils
-- Description: Provides a few tools to write new layouts.
-- Copyright: (c) Mesabloo and contributors, 2021-
-- License: BSD-3
-- Stability: experimental
-- Portability: Portable
--
-- This module will be useful only to people writing new layout packages for Diagnose.
-- Under normal use, it should not be imported nor used.
module Error.Diagnose.Utils where

import Control.Arrow ((&&&))
import Data.Array (Ix)
import Data.Array.IArray (IArray)
import qualified Data.Array.IArray as Array
import Data.Array.Unboxed (UArray, listArray)
import Data.Bifunctor (second)
import Data.Char.WCWidth (wcwidth)
import Data.Foldable (find, fold)
import Data.Functor ((<&>))
import qualified Data.HashMap.Lazy as HashMap
import Data.List (intersperse)
import Data.Some (Some (..), mapSome, withSome)
import qualified Data.Text as Text
import Error.Diagnose.Layout (FileMap)
import Error.Diagnose.Position (SourceRange (..), begin)
import Error.Diagnose.Pretty (Doc, Pretty (..), annotate, space)
import Error.Diagnose.Report (Marker (..))
import Prettyprinter.Internal.Type (Doc (..))

-- | The width table is the mapping between indices in the source code, and width of the characters
--   at this index.
--   It can be thought of as mapping @i@ to @wcwidth(code[i])@.
type WidthTable = UArray Int Int

-- | Retrieves a specific line from within the file map, with colors added to it.
fetchLine ::
  forall msg ann.
  Pretty msg =>
  -- | The mapping between file names and file contents.
  FileMap ->
  -- | The path to the file to get the line from.
  FilePath ->
  -- | The line number corresponding to the line to get.
  Int ->
  -- | The number of spaces used to show a TAB character.
  Int ->
  -- | The annotation to add to the line when no line is found, in order to colorize the placeholder text (such as @"<no line>"@).
  (ann, String) ->
  -- | The annotation to add to the line when there are no markers underneath.
  ann ->
  -- | The annotation to add to the line when a marker is found underneath.
  (Some (Marker msg) -> ann) ->
  -- | All the markers present on that line.
  --
  --   We want to apply different treatments to simple markers and marker which can appear in notes.
  [Some (Marker msg)] ->
  -- | Returns the width table and the generated annotated 'Doc'ument.
  --
  --   Markers are also returned with the position adjusted to fit the code perfectly (in case
  --   of code modification with 'AddCode' markers).
  --   Note that this is the case only when added code does not contain new lines.
  --   If that's not the case, character substitution will be performed to removed those characters.
  (WidthTable, [Some (Marker msg)], Doc ann)
fetchLine files path line tabSize (noLineAnn, noLineText) codeAnn markerAnn markers =
  case safeArrayIndex (line - 1) =<< files HashMap.!? path of
    Nothing -> (mkWidthTable "", markers, annotate noLineAnn (pretty noLineText))
    Just code -> mkTuple3 (mkWidthTable code, colorizeCode 1 markers True code)
  where
    colorizeCode :: Integer -> [Some (Marker msg)] -> Bool -> String -> ([Some (Marker msg)], Doc ann)
    colorizeCode _ markers _ "" = (markers, mempty)
    colorizeCode n markers handleAdd (c : code)
      -- if we found that a note marker starts at this position,
      -- then we have to start potentially overriding the code (by adding chunks, for example).
      --
      -- in such case, we stop processing the code at this point, add the chunk of code
      -- and then continue.
      -- we also want to adjust the position
      | (True, Just (Some (AddCode (line, _) _ _ code'))) <- (handleAdd, specialNoteMarkerAt n markers) =
          let addLength = length code'
              -- register the length of the text, to add to all marker positions
              -- starting after @n@.

              -- WARN: don't partition here! we want to keep the original order of the markers.
              markers' =
                markers <&> \m ->
                  let Range (bl, bc) (el, ec) f = withSome m markerPosition

                      updatePos :: SourceRange -> Marker msg ki -> Marker msg ki
                      updatePos pos (Primary _ msg) = Primary pos msg
                      updatePos pos (Secondary _ msg) = Secondary pos msg
                      updatePos pos (Blank _) = Blank pos
                      updatePos (Range b _ _) (AddCode _ f l c) = AddCode b f l c
                      updatePos pos (RemoveCode _) = RemoveCode pos
                      updatePos pos (Annotate _ msg) = Annotate pos msg
                   in if bc < fromIntegral n
                        then m
                        else case m of
                          -- if this is the marker we are currently handling, do NOT shift it!
                          Some (AddCode {})
                            | bl == line && bc == fromIntegral n -> m
                          _ ->
                            let newStart = if bl == line then bc + addLength else bc
                                newEnd = if el == line then ec + addLength else ec
                                newPos = Range (bl, newStart) (el, newEnd) f
                             in mapSome (updatePos newPos) m

              substitute = fmap \case
                '\n' -> '␍'
                '\r' -> '␊'
                '\v' -> '␋'
                '\f' -> '␌'
                c -> c
           in colorizeCode n markers' False (substitute code' <> (c : code))
      | otherwise =
          let doc = ifTab (fold $ replicate tabSize space) pretty c

              allMarkersAtPos =
                flip filter markers \mark ->
                  let Range (bl, bc) (el, ec) _ = withSome mark markerPosition
                   in if bl == el
                        then -- for inline markers (those which the ending line is the same as the starting line),
                        -- we can just check if n is included inside @[start, end[@.
                          fromIntegral n >= bc && fromIntegral n < ec
                        else -- for multiline markers, it is a little bit more complicated.

                          (bl == line && fromIntegral n >= bc)
                            || (el == line && fromIntegral n < ec)
                            || (bl < line && el > line)

              colorizedDoc = maybe (annotate codeAnn) (annotate . markerAnn) (head' allMarkersAtPos) doc
           in second (colorizedDoc <>) $ colorizeCode (n + 1) markers True code

    specialNoteMarkerAt :: Integer -> [Some (Marker msg)] -> Maybe (Some (Marker msg))
    specialNoteMarkerAt n = find isNote
      where
        isNote :: Some (Marker msg) -> Bool
        isNote mark =
          let (cond, pos) = withSome mark (isNoteMarker &&& markerPosition)
           in cond && snd (begin pos) == fromIntegral n

        isNoteMarker :: Marker msg kind -> Bool
        isNoteMarker (AddCode {}) = True
        isNoteMarker (RemoveCode {}) = True
        isNoteMarker (Annotate {}) = True
        isNoteMarker _ = False

    ifTab :: a -> (Char -> a) -> Char -> a
    ifTab a _ '\t' = a
    ifTab _ f c = f c

    mkWidthTable :: String -> WidthTable
    mkWidthTable s = listArray (1, length s) (ifTab tabSize wcwidth <$> s)

    head' :: [a] -> Maybe a
    head' [] = Nothing
    head' (x : _) = Just x
    {-# INLINE head' #-}

    mkTuple3 :: (a, (b, c)) -> (a, b, c)
    mkTuple3 (x, (y, z)) = (x, y, z)
    {-# INLINE mkTuple3 #-}

-- | Retrieves the position a marker is spanning.
markerPosition :: Marker msg k -> SourceRange
markerPosition (Primary pos _) = pos
markerPosition (Secondary pos _) = pos
markerPosition (Blank pos) = pos
markerPosition (AddCode (line, col) file len _) = Range (line, col) (line, col + len) file
markerPosition (RemoveCode pos) = pos
markerPosition (Annotate pos _) = pos

-- | Safely index into an array-like.
safeArrayIndex :: (Ix i, IArray a e) => i -> a i e -> Maybe e
safeArrayIndex i a
  | Array.inRange (Array.bounds a) i = Just (a Array.! i)
  | otherwise = Nothing

-- | Retrieves the message held by a marker.
markerMessage :: Pretty msg => Marker msg k -> Maybe msg
markerMessage (Primary _ m) = m
markerMessage (Secondary _ m) = m
markerMessage (Blank _) = Nothing
markerMessage (AddCode {}) = Nothing
markerMessage (RemoveCode _) = Nothing
markerMessage (Annotate _ m) = m

-- | Replaces lines in a 'Doc'ument with a given 'Doc'ument.
replaceLinesWith ::
  -- | The replacement document, to e.g. add a prefix after a new line.
  Doc ann ->
  -- | A function to be applied to every piece of text gathered from breaking
  --   a single 'Text' on every literal newline characters.
  (Doc ann -> Doc ann) ->
  -- | The document to replace new lines with.
  Doc ann ->
  Doc ann
replaceLinesWith repl t = go
  where
    go Line = repl
    go Fail = Fail
    go Empty = Empty
    go (Char '\n') = repl
    go (Char c) = Char c
    go (Text _ s) =
      mconcat $
        intersperse repl $
          t . uncurry Text . (Text.length &&& id)
            <$> Text.split (== '\n') s
    go (FlatAlt f d) = FlatAlt (go f) (go d)
    go (Cat c d) = Cat (go c) (go d)
    go (Nest n d) = Nest n (go d)
    go (Union c d) = Union (go c) (go d)
    go (Column f) = Column (go . f)
    go (Nesting f) = Nesting (go . f)
    go (Annotated ann doc) = Annotated ann (go doc)
    go (WithPageWidth f) = WithPageWidth (go . f)
