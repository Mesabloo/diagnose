-- |
-- Module      : Error.Diagnose.Layout.Ariadne.Config
-- Description : Character configuration definition for the Ariadne layout.
-- Copyright   : (c) Mesabloo and contributors, 2022-
-- License     : BSD3
-- Stability   : experimental
-- Portability : Portable
module Error.Diagnose.Layout.Ariadne.Config where

-- | A character configuration, to simply switch between Unicode and ASCII without having
--   to hardcode those characters inside the source code.
data Configuration = Config
  { -- | This is the prefix used when printing a file location which isn't at the top of the report.
    --
    --   For example, when outputting notes with markers, this is the character used to link the arrow to the rule.
    --
    --   In the default style, this is @├@.
    inlineFileArrowPrefix :: Char,
    -- | The prefix used when the file location is at the very top of the report.
    --
    --   In the default style, this is @╭@.
    startFileArrowPrefix :: Char,
    -- | The tip of the arrow right before the file position.
    --
    --   In the default style, this is @▶@.
    fileArrowTip :: Char,
    -- | The character used to draw the rule when it is horizontal.
    --
    --   In the default style, this is @─@.
    horizontalRule :: Char,
    -- | The character used to draw the rule when it is vertical.
    --
    --   In the default style, this is @│@.
    verticalRule :: Char,
    -- | The first character of an 'Error.Diagnose.Report.AddCode' marker rule.
    --
    --   In the default style, this is @⁺@.
    additionMarkerStart :: Char,
    -- | The character used after 'additionMarkerStart' if the marker is more than one column long.
    --
    --   In the default style, this is the same as 'additionMarkerStart'.
    additionMarkerMiddle :: Char,
    -- | The first character of an 'Error.Diagnose.Report.RemoveCode' marker rule.
    --
    --   In the default style, this is @⁻@.
    deletionMarkerStart :: Char,
    -- | Similar to 'additionMarkerMiddle' but for 'Error.Diagnose.Report.RemoveCode' marker rules.
    --
    --   In the default style, this is the same as 'deletionMarkerStart'.
    deletionMarkerMiddle :: Char,
    -- | The first character of any marker rule.
    --   Note that it will not be used (instead, 'markerMiddle' will be) if the marker does not have any message to be shown.
    --
    --   In the default style, this is @┬@.
    markerStart :: Char,
    -- | The character to be used to render a marker rule after 'markerStart'.
    --
    --   In the default style, this is @─@.
    markerMiddle :: Char,
    -- | The character which is used to output the head of a multiline marker.
    --
    --   In the default style, this is @┤@.
    multilineMarkerHead :: Char,
    -- | The first character which is output underneath a marker rule, before the message.
    --   Together with 'markerMessagePrefixMiddle' and 'markerMessagePrefixEnd', this forms the connection between
    --   the marker rule and the marker message.
    --
    --   In the default style, this is @╰@.
    markerMessagePrefixStart :: Char,
    -- | The character used to prolongate the message prefix (in case we need to align entries).
    --
    --   In the default style, this is @─@.
    markerMessagePrefixMiddle :: Char,
    -- | The last character which is output before the marker message.
    --
    --   In the default style, this is @╸@.
    markerMessagePrefixEnd :: Char,
    -- | The character to be used to start a new multiline group.
    --
    --   In the default style, this is @╭@.
    multilineMarkerStart :: Char,
    -- | The character used when connecting multiple multiline markers in the same group.
    --
    --   In the default style, this is @├@.
    multilineMarkerMiddle :: Char,
    -- | This is the character used in the rule before markers, as well as between non-consecutive groups of lines.
    --
    --   In the default style, this is @•@.
    dotRule :: Char,
    -- | The character used to connect the end of the rule with the rule itself.
    --
    --   In the default style, this is @╯@.
    endRuleSuffix :: Char,
    -- | Similar to 'markerMessagePrefixStart' but for multiline markers, when the message is not the last one.
    --
    --   In the default style, this is @├@.
    multilineMarkerPrefixStart :: Char
  }

-- | The character 'Configuration' for Unicode style.
unicodeConfiguration :: Configuration
unicodeConfiguration =
  Config
    { inlineFileArrowPrefix = '├',
      startFileArrowPrefix = '╭',
      fileArrowTip = '▶',
      horizontalRule = '─',
      verticalRule = '│',
      additionMarkerStart = '⁺',
      additionMarkerMiddle = '⁺',
      deletionMarkerStart = '⁻',
      deletionMarkerMiddle = '⁻',
      markerStart = '┬',
      markerMiddle = '─',
      multilineMarkerHead = '┤',
      markerMessagePrefixStart = '╰',
      markerMessagePrefixMiddle = '─',
      markerMessagePrefixEnd = '╸',
      multilineMarkerStart = '╭',
      multilineMarkerMiddle = '├',
      dotRule = '•',
      endRuleSuffix = '╯',
      multilineMarkerPrefixStart = '├'
    }

-- | The character 'Configuration' for ASCII-style.
asciiConfiguration :: Configuration
asciiConfiguration =
  Config
    { inlineFileArrowPrefix = '+',
      startFileArrowPrefix = '+',
      fileArrowTip = '>',
      horizontalRule = '-',
      verticalRule = '|',
      additionMarkerStart = '+',
      additionMarkerMiddle = '+',
      deletionMarkerStart = '-',
      deletionMarkerMiddle = '-',
      markerStart = '^',
      markerMiddle = '~',
      multilineMarkerHead = '>',
      markerMessagePrefixStart = '`',
      markerMessagePrefixMiddle = '-',
      markerMessagePrefixEnd = '-',
      multilineMarkerStart = '+',
      multilineMarkerMiddle = '+',
      dotRule = ':',
      endRuleSuffix = '+',
      multilineMarkerPrefixStart = '|'
    }

-- | Switches between 'unicodeConfiguration' or 'asciiConfiguration' depending on whether the first argument
--   is @'True'@ or @'False'@.
configuration :: Bool -> Configuration
configuration True = unicodeConfiguration
configuration False = asciiConfiguration
