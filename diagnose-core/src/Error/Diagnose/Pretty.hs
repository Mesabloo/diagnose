{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

-- |
-- Module: Error.Diagnose.Pretty
-- Description: Provides pretty-printing tools.
-- Copyright: (c) Mesabloo and contributors, 2021-
-- License: BSD-3
-- Stability: experimental
-- Portability: Portable
--
-- This module exports pretty-printing tools so that you don't need to directly
-- depend on them in your project.
module Error.Diagnose.Pretty (module Export) where

import Prettyprinter as Export
import Prettyprinter.Render.Terminal as Export
