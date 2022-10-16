module Error.Diagnose.Layout.CodeSpanReporting (codespanReportingLayout, defaultStyle) where

import Error.Diagnose.Diagnostic (filesOf, reportsOf)
import Error.Diagnose.Layout (Layout)
import Error.Diagnose.Style (Style, reAnnotate)

import Error.Diagnose (Color (..), bold, color, colorDull)
import qualified Error.Diagnose.Layout.CodeSpanReporting.Config as R
import qualified Error.Diagnose.Layout.CodeSpanReporting.Render as R

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
codespanReportingLayout :: Layout R.Annotation msg
codespanReportingLayout withUnicode tabSize diag
  = foldMap (R.report (filesOf diag) chars tabSize) (reportsOf diag)
  -- fold . intersperse hardline $ prettyReport (filesOf diag) withUnicode tabSize <$> reportsOf diag
  where chars = if withUnicode then R.unicodeChars else R.asciiChars
{-# INLINE codespanReportingLayout #-}

defaultStyle :: Style R.Annotation
defaultStyle = reAnnotate \case
  R.Header R.Bug       -> bold <> color Red
  R.Header R.Error     -> bold <> color Red
  R.Header R.Warning   -> bold <> color Yellow
  R.Header R.Note      -> bold <> color Green
  R.Header R.Help      -> bold <> color Cyan
  R.HeaderMessage      -> bold <> color White
  R.SourceBorder       -> colorDull Cyan -- Blue
  R.NoteBullet         -> colorDull Cyan -- Blue
  R.LineNumber         -> colorDull Cyan -- Blue
  R.SourceTint sev sty -> marker sev sty True
  R.MarkerTint sev sty -> marker sev sty False
  where marker R.Bug     R.SPrimary   _     = colorDull Red
        marker R.Error   R.SPrimary   _     = colorDull Red
        marker R.Warning R.SPrimary   _     = colorDull Yellow
        marker R.Note    R.SPrimary   _     = colorDull Green
        marker R.Help    R.SPrimary   _     = colorDull Cyan
        marker _         R.SBlank     _     = mempty
        marker _         R.SAdd       _     = color Green
        marker _         R.SRemove    _     = color Red
        marker _         R.SSecondary True  = colorDull White
        marker _         R.SSecondary False = colorDull Cyan -- Blue
