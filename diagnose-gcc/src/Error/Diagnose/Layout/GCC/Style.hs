{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module Error.Diagnose.Layout.GCC.Style where

import Error.Diagnose.Pretty (Color (..), bold, color, colorDull)
import Error.Diagnose.Report (Severity (..))
import Error.Diagnose.Style (IsAnnotation (..), Style, reAnnotate)

data GccAnnotation
  = PrimaryTint Severity
  | SecondaryTint
  | AdditionTint
  | RemovalTint
  | NoteTint
  | FilePosition
  | NoLineTint
  | Marker GccAnnotation
  | CodeTint
  | Header GccAnnotation

instance {-# OVERLAPPABLE #-} IsAnnotation GccAnnotation where
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

-- | TODO
gccStyle :: Style GccAnnotation
gccStyle = reAnnotate mkColor
