{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module Error.Diagnose.Layout.Typescript.Style where

import Error.Diagnose.Pretty (Color (..), bgColor, bgColorDull, bold, color, colorDull)
import Error.Diagnose.Report (Severity (..))
import Error.Diagnose.Style (IsAnnotation (..), Style, reAnnotate)

data TypescriptAnnotation
  = PrimaryTint Severity
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

instance {-# OVERLAPPABLE #-} IsAnnotation TypescriptAnnotation where
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
