{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE Safe #-}

-- |
-- Module      : Error.Diagnose.Layout.Ariadne.Style
-- Description : Annotations and default color definitions for the Ariadne layout.
-- Copyright   : (c) Mesabloo and contributors, 2022-
-- License     : BSD3
-- Stability   : experimental
-- Portability : Portable
--
-- This modules contains the definition for coloring annotations as well as a default color scheme for them.
-- If this default color scheme does not suit you well, please define your own instance of 'IsAnnotation' 'AriadneAnnotation'
-- and mark it as @{-# OVERLAPPING #-}@.
module Error.Diagnose.Layout.Ariadne.Style where

import Error.Diagnose.Pretty (Color (..), bold, color, colorDull)
import Error.Diagnose.Report (Severity (..))
import Error.Diagnose.Style (IsAnnotation (..), Style, reAnnotate)

-- | A collection of styling annotations which are semantically better than 'Error.Diagnose.Pretty.AnsiStyle'
--   and allow for quick modification without needing to go into the internals of the library.
--
--   You can use the typeclass 'IsAnnotation' to implement a conversion to 'Error.Diagnose.Pretty.AnsiStyle'.
data AriadneAnnotation
  = -- | The color used both for the header and the 'Error.Diagnose.Report.Primary' markers.
    PrimaryTint Severity
  | -- | The color used for 'Error.Diagnose.Report.Secondary' and 'Error.Diagnose.Report.Annotate' markers.
    SecondaryTint
  | -- | The color used for 'Error.Diagnose.Report.AddCode' markers.
    AdditionTint
  | -- | The color used for 'Error.Diagnose.Report.RemoveCode' markers.
    RemovalTint
  | -- | The style of the rule separating line numbers from the code and markers.
    Rule
  | -- | The color used for the line numbers in the left gutter.
    LineNumberTint
  | -- | The color used for messages of notes and hints.
    NoteTint
  | -- | The color used for file positions.
    File
  | -- | An alteration of an annotation which allows to add modifiers on top of a 'PrimaryTint'.
    --
    --   This is used for the @[error]@ part of the report.
    Header AriadneAnnotation
  | -- | The color used when @\<no-line\>@ appears instead of a line of code.
    NoLineTint
  | -- | The style used for the underlining rules of markers.
    Marker AriadneAnnotation
  | -- | The default color of code when there are no markers underneath.
    CodeTint

-- | Can be overlapped as needed.
instance {-# OVERLAPPABLE #-} IsAnnotation AriadneAnnotation where
  mkColor = \case
    PrimaryTint Error -> color Red
    PrimaryTint Warning -> color Yellow
    PrimaryTint Critical -> colorDull Red
    SecondaryTint -> colorDull Blue
    AdditionTint -> colorDull Green
    RemovalTint -> colorDull Cyan
    Rule -> bold <> colorDull Black
    LineNumberTint -> colorDull White
    NoteTint -> color Cyan
    File -> bold <> colorDull Green
    Header ann -> bold <> mkColor ann
    NoLineTint -> bold <> colorDull Magenta
    Marker CodeTint -> mkColor CodeTint
    Marker ann -> bold <> mkColor ann
    CodeTint -> color White

-- | The default style for the layout.
--
-- - 'Error.Diagnose.Report.Primary' markers are 'bold' and colored in:
--     - 'Red' if the report is an 'Error' report;
--     - 'Yellow' if the report is a 'Warning' report;
--     - A 'Error.Diagnose.Pretty.Dull' 'Red' if the report is for a critical failure.
-- - 'Error.Diagnose.Report.Secondary' and 'Error.Diagnose.Report.Annotate' markers are colored in a 'Error.Diagnose.Pretty.Dull' 'Blue';
-- - 'Error.Diagnose.Report.AddCode' markers are colored in a 'Error.Diagnose.Pretty.Dull' 'Green';
-- - 'Error.Diagnose.Report.RemoveCode' markers are colored in a 'Error.Diagnose.Pretty.Dull' 'Cyan';
-- - The rule is output in a 'bold' 'Error.Diagnose.Pretty.Dull' 'Black'.
--   However, line numbers are shown in a 'Error.Diagnose.Pretty.Dull' 'White';
-- - 'Error.Diagnose.Report.Note' messages are output in 'Cyan';
-- - File positions are output in a 'bold' 'Error.Diagnose.Pretty.Dull' 'Green';
-- - Code without markers is simply output 'White'.
ariadneStyle :: Style AriadneAnnotation
ariadneStyle = reAnnotate mkColor
