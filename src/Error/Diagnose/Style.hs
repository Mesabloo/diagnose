{-# LANGUAGE DeriveGeneric #-}
-- |
-- Module      : Error.Diagnose.Style
-- Description : Custom style definitions
-- Copyright   : (c) Mesabloo, 2021-2022
-- License     : BSD3
-- Stability   : experimental
-- Portability : Portable
module Error.Diagnose.Style
  ( -- * Defining new style
    Annotation (..),
    Style,
    -- $defining_new_styles

    -- * Default style specification
    defaultStyle,

    -- * Re-exports
    reAnnotate,
  )
where

import GHC.Generics
import Prettyprinter (Doc, reAnnotate)
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..), bold, color, colorDull)

-- $defining_new_styles
--
-- Defining new color styles (one may call them "themes") is actually rather easy.
--
-- A 'Style' is a function from an annotated 'Doc'ument to another annotated 'Doc'ument.
-- Note that only the annotation type changes, hence the need of only providing a unidirectional mapping between those.
--
-- 'Annotation's are used when creating a 'Doc'ument and are simply placeholders to specify custom colors.
-- 'AnsiStyle' is the concrete annotation to specify custom colors when rendering a 'Doc'ument.
--
-- One may define additional styles as follows:
--
-- > myNewCustomStyle :: Style
-- > myNewCustomStyle = reAnnotate \case
-- >   -- all cases for all annotations
--
-- For simplicity's sake, a default style is given as 'defaultStyle'.

-- | Some annotations as placeholders for colors in a 'Doc'.
data Annotation
  = -- | The color of 'Error.Diagnose.Report.This' markers, depending on whether the report is an error
    --   report or a warning report.
    ThisColor
      Bool
  | -- | The color of 'Error.Diagnose.Report.Maybe' markers.
    MaybeColor
  | -- | The color of 'Error.Diagnose.Report.Where' markers.
    WhereColor
  | -- | The color for hints.
    --
    --   Note that the beginning @Hint:@ text will always be in bold.
    HintColor
  | -- | The color for file names.
    FileColor
  | -- | The color of the rule separating the code/markers from the line numbers.
    RuleColor
  | -- | The color of the @[error]@/@[warning]@ at the top, depending on whether
    --   this is an error or warning report.
    KindColor
      Bool
  | -- | The color in which to output the @<no line>@ information when the file was not found.
    NoLineColor
  | -- | Additional style to apply to marker rules (e.g. bold) on top of some
    --   already processed color annotation.
    MarkerStyle
      Annotation
  | -- | The color of the code when no marker is present.
    CodeStyle
  deriving (Eq, Ord, Show, Generic)
-- | A style is a function which can be applied using 'reAnnotate'.
--
--   It transforms a 'Doc'ument containing 'Annotation's into a 'Doc'ument containing
--   color information.
type Style = Doc Annotation -> Doc AnsiStyle

-------------------------------------------

-- | The default style for diagnostics, where:
--
--   * 'Error.Diagnose.Report.This' markers are colored in red for errors and yellow for warnings
--   * 'Error.Diagnose.Report.Where' markers are colored in dull blue
--   * 'Error.Diagnose.Report.Maybe' markers are colored in magenta
--   * Marker rules are of the same color of the marker, but also in bold
--   * Hints are output in cyan
--   * The left rules are colored in bold black
--   * File names are output in dull green
--   * The @[error]@/@[warning]@ at the top is colored in red for errors and yellow for warnings
--   * The code is output in normal white
defaultStyle :: Style
defaultStyle = reAnnotate style
  where
    style = \case
      ThisColor isError -> color if isError then Red else Yellow
      MaybeColor -> color Magenta
      WhereColor -> colorDull Blue
      HintColor -> color Cyan
      FileColor -> bold <> colorDull Green
      RuleColor -> bold <> color Black
      KindColor isError -> bold <> style (ThisColor isError)
      NoLineColor -> bold <> colorDull Magenta
      MarkerStyle st ->
        let ann = style st
         in if ann == style CodeStyle
              then ann
              else bold <> ann
      CodeStyle -> color White
