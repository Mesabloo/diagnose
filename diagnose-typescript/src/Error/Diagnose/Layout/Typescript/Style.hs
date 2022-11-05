{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

-- |
-- Module      : Error.Diagnose.Layout.GCC.Style
-- Description : Annotations and default color definitions for the Typescript layout.
-- Copyright   : (c) Mesabloo and contributors, 2022-
-- License     : BSD3
-- Stability   : experimental
-- Portability : Portable
--
-- This modules contains the definition for coloring annotations as well as a default color scheme for them.
module Error.Diagnose.Layout.Typescript.Style where

import Error.Diagnose.Pretty (Color (..), bgColor, bgColorDull, bold, color, colorDull)
import Error.Diagnose.Report (Severity (..))
import Error.Diagnose.Style (IsAnnotation (..), Style, reAnnotate)

-- | An abstract representation of coloring information for the Typescript layout.
data TypescriptAnnotation
  = -- | TODO
    PrimaryTint Severity
  | SecondaryTint
  | AdditionTint
  | RemovalTint
  | NoteTint
  | FilePosition
  | FileName
  | LineNumber Severity
  | NoLineTint
  | Marker TypescriptAnnotation
  | CodeTint
  | ErrorCode
  | Header TypescriptAnnotation

instance IsAnnotation TypescriptAnnotation where
  mkColor = \case
    PrimaryTint Error -> color Red
    PrimaryTint Warning -> color Yellow
    PrimaryTint Critical -> colorDull Red <> bold
    SecondaryTint -> colorDull Cyan
    FileName -> colorDull Blue
    FilePosition -> colorDull Yellow
    LineNumber Error -> bgColor Red <> colorDull Black
    LineNumber Warning -> bgColor Yellow <> colorDull Black
    LineNumber Critical -> bgColorDull Red <> colorDull Black
    NoLineTint -> bold <> colorDull Magenta
    Marker CodeTint -> mkColor CodeTint
    Marker ann -> bold <> mkColor ann
    CodeTint -> color White
    ErrorCode -> colorDull Black
    Header ann -> bold <> mkColor ann
    AdditionTint -> color Green
    RemovalTint -> color Green
    NoteTint -> color Cyan

-- | TODO
typescriptStyle :: Style TypescriptAnnotation
typescriptStyle = reAnnotate mkColor
