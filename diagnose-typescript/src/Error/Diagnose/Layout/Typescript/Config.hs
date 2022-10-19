module Error.Diagnose.Layout.Typescript.Config where

data Configuration = Config
  { -- | The character used to underline in most markers.
    markerChar :: Char,
    -- | The character used to underline code additions.
    additionChar :: Char,
    -- | The character used to underline code removals.
    removalChar :: Char
  }

unicodeConfiguration :: Configuration
unicodeConfiguration =
  Config
    { markerChar = '─',
      additionChar = '⁺',
      removalChar = '⁻'
    }

asciiConfiguration :: Configuration
asciiConfiguration =
  Config
    { markerChar = '~',
      additionChar = '+',
      removalChar = '-'
    }

-- | Returns the correct configuration depending on whether we want unicode characters or not.
configuration :: Bool -> Configuration
configuration True = unicodeConfiguration
configuration False = asciiConfiguration
