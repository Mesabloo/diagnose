module Text.Diagnose.Format.JSON where

import Text.PrettyPrint.ANSI.Leijen

class PrettyJSON a where
  -- | Prettifies a value into a JSON representation.
  prettyJSON :: a -> Doc
