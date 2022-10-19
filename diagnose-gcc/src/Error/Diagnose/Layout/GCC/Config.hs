module Error.Diagnose.Layout.GCC.Config where

-- | A character configuration.
data Configuration = Config
  { -- | The character to use when starting a marker and the width of the marker is more than 1.
    startMarker :: Char,
    -- | The character to use in the middle of the marker, if the marker is more than 2 characters long.
    middleMarker :: Char,
    -- | The character used at the end of the marker, if the marker ends on the same line.
    endMarker :: Char,
    -- | The character to use when the marker has a width of exactly 1.
    --
    --   Note: Unicode characters whose display width are 2 or more will not be underlined with this character,
    --         even if the marker has a specified width of 1 unicode code point.
    singleMarker :: Char,
    -- | Same as 'startMarker' but for markers underneath added code.
    startAddition :: Char,
    -- | Same as 'middleMarker' but for markers underneath added code.
    middleAddition :: Char,
    -- | Same as 'endMarker' but for markers underneath added code.
    endAddition :: Char,
    -- | Same as 'singleMarker' but for markers underneath added code.
    singleAddition :: Char,
    -- | Same as 'startMarker' but for code to be deleted.
    startRemoval :: Char,
    -- | Same as 'middleMarker' but for code to be deleted.
    middleRemoval :: Char,
    -- | Same as 'middleRemoval' but for code to be deleted.
    endRemoval :: Char,
    -- | Same as 'singleRemove' but for code to be deleted.
    singleRemoval :: Char,
    -- | The ellipsis string which is put at the end of a marker if it spans across multiple lines.
    ellipsis :: String
  }

-- | A character 'Configuration' for Unicode-style.
unicodeConfiguration :: Configuration
unicodeConfiguration =
  Config
    { startMarker = '└',
      middleMarker = '─',
      endMarker = '┘',
      singleMarker = '┴',
      startAddition = '⁺',
      middleAddition = '⁺',
      endAddition = '⁺',
      singleAddition = '⁺',
      startRemoval = '⁻',
      middleRemoval = '⁻',
      endRemoval = '⁻',
      singleRemoval = '⁻',
      ellipsis = "…"
    }

-- | A character 'Configuration' for ASCII-style.
asciiConfiguration :: Configuration
asciiConfiguration =
  Config
    { startMarker = '^',
      middleMarker = '~',
      endMarker = '~',
      singleMarker = '^',
      startAddition = '+',
      middleAddition = '+',
      endAddition = '+',
      singleAddition = '^',
      startRemoval = '-',
      middleRemoval = '-',
      endRemoval = '-',
      singleRemoval = '-',
      ellipsis = "..."
    }

-- | Returns the correct character configuration to use in a report.
configuration ::
  -- | Whether we want unicode characters or not.
  Bool ->
  Configuration
configuration True = unicodeConfiguration
configuration False = asciiConfiguration
