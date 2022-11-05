{-# LANGUAGE FlexibleContexts #-}
module Error.Diagnose.Layout.CodeSpanReporting
  ( codespanReportingLayout
  , codespanReportingStyle
  ) where

import Error.Diagnose.Diagnostic (Diagnostic, filesOf, reportsOf)

import Error.Diagnose (AnsiStyle, IsAnnotation (mkColor))
import qualified Error.Diagnose.Layout.CodeSpanReporting.Config as R
import qualified Error.Diagnose.Layout.CodeSpanReporting.Render as R
import Prettyprinter (Doc, Pretty, reAnnotate)

-- | Pretty prints a 'Diagnostic' into a 'Doc'ument that can be output using 'hPutDoc'.
--
--   Colors are put by default.
--   If you do not want these, just 'unAnnotate' the resulting document like so:
--
--   >>> let doc = unAnnotate (prettyDiagnostic withUnicode tabSize diagnostic)
--
--   Changing the style is also rather easy:
--
--   >>> let myCustomStyle :: Style = _
--   >>> let doc = myCustomStyle (prettyDiagnostic withUnicode tabSize diagnostic)
codespanReportingLayout :: Pretty msg => Bool -> Int -> Diagnostic msg -> Doc R.Annotation
codespanReportingLayout withUnicode tabSize diag
  = foldMap (R.report (filesOf diag) chars tabSize) (reportsOf diag)
  where chars = if withUnicode then R.unicodeChars else R.asciiChars
{-# INLINE codespanReportingLayout #-}

codespanReportingStyle :: Doc R.Annotation -> Doc AnsiStyle
codespanReportingStyle = reAnnotate mkColor
