{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

-- |
-- Module      : Error.Diagnose.Layout.GCC.Style
-- Description : Annotations and default color definitions for the GCC layout.
-- Copyright   : (c) Mesabloo and contributors, 2022-
-- License     : BSD3
-- Stability   : experimental
-- Portability : Portable
--
-- This modules contains the definition for coloring annotations as well as a default color scheme for them.
module Error.Diagnose.Layout.GCC.Style where

import Error.Diagnose.Pretty (Color (..), bold, color, colorDull)
import Error.Diagnose.Report (Severity (..))
import Error.Diagnose.Style (IsAnnotation (..), Style, reAnnotate)

-- | The type of abstract coloring annotations for the GCC layout.
data GccAnnotation
  = -- | The main tint for 'Error.Diagnose.Report.Primary' markers and their associated header.
    PrimaryTint Severity
  | -- | The color for 'Error.Diagnose.Report.Secondary' and 'Error.Diagnose.Report.Annotate' markers.
    SecondaryTint
  | -- | The color for 'Error.Diagnose.Report.AddCode' markers.
    AdditionTint
  | -- | The color for 'Error.Diagnose.Report.RemoveCode' markers.
    RemovalTint
  | -- | The color for the @note:@ and @help:@ headers in notes.
    NoteTint
  | -- | The style for file positions.
    FilePosition
  | -- | The color of the @\<no-line\>@ placeholder if a line is not found.
    NoLineTint
  | -- | The style of underlining marker, related to their original color.
    Marker GccAnnotation
  | -- | The tint of the code when there are no markers underneath.
    CodeTint
  | -- | The style of the @error:@, @warning:@, @critical:@ and @note:@ section headers.
    Header GccAnnotation

instance IsAnnotation GccAnnotation where
  mkColor = \case
    PrimaryTint Error -> color Red
    PrimaryTint Warning -> color Magenta
    PrimaryTint Critical -> colorDull Red <> bold
    SecondaryTint -> colorDull Cyan
    AdditionTint -> color Green
    RemovalTint -> color Green
    FilePosition -> bold <> color White
    NoLineTint -> bold <> colorDull Magenta
    Marker CodeTint -> mkColor CodeTint
    Marker ann -> bold <> mkColor ann
    CodeTint -> color White
    NoteTint -> color Cyan
    Header ann -> bold <> mkColor ann

-- | The default style for the GCC layout, where
--
--   - 'Error.Diagnose.Report.Primary' markers are output in
--       * 'Red' if the severity of the report is 'Error';
--       * 'Magenta' if the severity of the report is 'Warning';
--       * a 'bold' 'Error.Diagnose.Pretty.Dull' 'Red' if the severity is 'Critical';
--   - 'Error.Diagnose.Report.Secondary' markers are output in a 'Error.Diagnose.Pretty.Dull' 'Cyan';
--   - 'Error.Diagnose.Report.AddCode' markers are output in 'Green';
--   - 'Error.Diagnose.Report.RemoveCode' markers are also output in 'Green';
--   - File positions are output in a 'bold' 'White';
--   - The @\<no-line\>@ line is output in a 'bold' 'Error.Diagnose.Pretty.Dull' 'Magenta';
--   - The source code is output in 'White'.
gccStyle :: Style GccAnnotation
gccStyle = reAnnotate mkColor
