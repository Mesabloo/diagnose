{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.Diagnose.Format.Text where

import Text.PrettyPrint.ANSI.Leijen

class PrettyText a where
  -- | Prettifies into a simple 'Doc'.
  prettyText :: a -> Doc

instance PrettyText String where
  prettyText = pretty

instance PrettyText Integer where
  prettyText = integer

instance PrettyText Int where
  prettyText = int

instance PrettyText Char where
  prettyText = text . (: [])
