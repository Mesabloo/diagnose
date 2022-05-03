module Error.Diagnose.Style where

import Prettyprinter (Doc, reAnnotate)
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..), bold, color, colorDull)

-- | Some annotations as placeholders for colors in a 'Doc'.
data Annotation
  = -- | The color of 'This' markers, depending on whether the report is an error
    --   report or a warning report.
    ThisColor
      Bool
  | -- | The color of 'Maybe' markers.
    MaybeColor
  | -- | The color of 'Where' markers.
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

-- | A style is a function which can be applied using 'reAnnotate'.
--
--   It transforms a 'Doc'ument containing 'Annotation's into a 'Doc'ument containing
--   color information.
type Style = Doc Annotation -> Doc AnsiStyle

-------------------------------------------

-- | The default style for diagnostics, where:
--
--   * 'This' markers are colored in red for errors and yellow for warnings
--   * 'Where' markers are colored in dull blue
--   * 'Maybe' markers are colored in magenta
--   * Hints are output in cyan
--   * The left rules are colored in bold black
--   * File names are output in dull green
--   * The @[error]@ at the top is colored in red for errors and yellow for warnings
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
      KindColor isError -> bold <> color if isError then Red else Yellow
      NoLineColor -> bold <> colorDull Magenta
      MarkerStyle st -> bold <> style st
