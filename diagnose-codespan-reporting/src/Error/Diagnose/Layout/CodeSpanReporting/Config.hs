module Error.Diagnose.Layout.CodeSpanReporting.Config where

data Chars = Chars
  -- | The characters to use for the top-left border of the snippet.
  --   Defaults to: @┌─@ or @-->@ with asciiChars.
  { cSnippetStart :: String
  -- | The character to use for the left border of the source.
  --   Defaults to: @│@ or @|@ with asciiChars.
  , cSourceBorderLeft :: Char
  -- | The character to use for the left border break of the source.
  --   Defaults to: @·@ or @.@ with asciiChars.
  , cSourceBorderLeftBreak :: Char
  -- | The character to use for the note bullet.
  --   Defaults to: @=@.
  , cNoteBullet :: Char
  -- | The character to use for marking a single-line primary label.
  --   Defaults to: @^@.
  , cSinglePrimaryCaret :: Char
  -- | The character to use for marking a single-line secondary label.
  --   Defaults to: @-@.
  , cSingleSecondaryCaret :: Char
  -- | The character to use for marking the start of a multi-line primary label.
  --   Defaults to: @^@.
  , cMultiPrimaryCaretStart :: Char
  -- | The character to use for marking the end of a multi-line primary label.
  --   Defaults to: @^@.
  , cMultiPrimaryCaretEnd :: Char
  -- | The character to use for marking the start of a multi-line secondary label.
  --   Defaults to: @\'@.
  , cMultiSecondaryCaretStart :: Char
  -- | The character to use for marking the end of a multi-line secondary label.
  --   Defaults to: @\'@.
  , cMultiSecondaryCaretEnd :: Char
  -- | The character to use for the top-left corner of a multi-line label.
  --   Defaults to: @╭@ or @/@ with asciiChars.
  , cMultiTopLeft :: Char
  -- | The character to use for the top of a multi-line label.
  --   Defaults to: @─@ or @-@ with asciiChars.
  , cMultiTop :: Char
  -- | The character to use for the bottom-left corner of a multi-line label.
  --   Defaults to: @╰@ or @\\@ with asciiChars.
  , cMultiBottomLeft :: Char
  -- | The character to use when marking the bottom of a multi-line label.
  --   Defaults to: @─@ or @-@ with asciiChars.
  , cMultiBottom :: Char
  -- | The character to use for the left of a multi-line label.
  --   Defaults to: @│@ or @|@ with asciiChars.
  , cMultiLeft :: Char
  -- | The character to use for the left of a pointer underneath a caret.
  --   Defaults to: @│@ or @|@ with asciiChars.
  , cPointerLeft :: Char
  } deriving (Show)

unicodeChars :: Chars
unicodeChars = Chars
  { cSnippetStart = "┌─"
  , cSourceBorderLeft = '│'
  , cSourceBorderLeftBreak = '·'
  , cNoteBullet = '='
  , cSinglePrimaryCaret = '^'
  , cSingleSecondaryCaret = '-'
  , cMultiPrimaryCaretStart = '^'
  , cMultiPrimaryCaretEnd = '^'
  , cMultiSecondaryCaretStart = '\''
  , cMultiSecondaryCaretEnd = '\''
  , cMultiTopLeft = '╭'
  , cMultiTop = '─'
  , cMultiBottomLeft = '╰'
  , cMultiBottom = '─'
  , cMultiLeft = '│'
  , cPointerLeft = '│'
  }

asciiChars :: Chars
asciiChars = Chars
  { cSnippetStart = "-->"
  , cSourceBorderLeft = '|'
  , cSourceBorderLeftBreak = '.'
  , cNoteBullet = '='
  , cSinglePrimaryCaret = '^'
  , cSingleSecondaryCaret = '-'
  , cMultiPrimaryCaretStart = '^'
  , cMultiPrimaryCaretEnd = '^'
  , cMultiSecondaryCaretStart = '\''
  , cMultiSecondaryCaretEnd = '\''
  , cMultiTopLeft = ' '
  , cMultiTop = '_'
  , cMultiBottomLeft = '|'
  , cMultiBottom = '_'
  , cMultiLeft = '|'
  , cPointerLeft = '|'
  }
