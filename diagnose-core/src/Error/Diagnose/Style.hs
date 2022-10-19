{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}

-- |
-- Module      : Error.Diagnose.Style
-- Description : Custom style definitions
-- Copyright   : (c) Mesabloo and contributors, 2021-
-- License     : BSD3
-- Stability   : experimental
-- Portability : Portable
module Error.Diagnose.Style
  ( -- * Defining new style
    IsAnnotation (..),
    Style,
    -- $defining_new_styles

    -- * Re-exports
    reAnnotate,
  )
where

import Error.Diagnose.Pretty (AnsiStyle, Doc, reAnnotate)

-- $defining_new_styles
--
-- Defining new color styles (one may call them "themes") is actually rather easy.
--
-- A 'Style' is a function from an annotated 'Doc'ument to another annotated 'Doc'ument.
-- Note that only the annotation type changes, hence the need of only providing a unidirectional mapping between those.
--
-- Annotations are used when creating a 'Doc'ument and are simply placeholders to specify custom colors.
-- 'AnsiStyle' is the concrete annotation to specify custom colors when rendering a 'Doc'ument.
--
-- One may define additional styles as follows:
--
-- > myNewCustomStyle :: Style MyAnnotation
-- > myNewCustomStyle = reAnnotate mkColor
--
-- Note that 'mkColor' comes from 'IsAnnotation', which must be instanciated for your own annotation type.
-- The purpose of 'mkColor' is to transform values from your custom annotation type to 'AnsiStyle' color descriptions.

-- | A style is a function which can be applied using 'reAnnotate'.
--
--   It transforms a 'Doc'ument containing annotations into a 'Doc'ument containing
--   color information.
type Style ann = IsAnnotation ann => Doc ann -> Doc AnsiStyle

-- | The class of annotation, allowing to map each to a given color style.
--
--   Every annotation used in a 'Style' must implemented this typeclass.
class IsAnnotation ann where
  -- | To be used with 'reAnnotate'.
  --
  --   Transforms the custom annotations into color annotations as described by 'AnsiStyle'.
  mkColor :: ann -> AnsiStyle
