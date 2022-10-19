{-# LANGUAGE Safe #-}

-- |
-- Module      : Error.Diagnose.Report
-- Description : Report definition and pretty printing
-- Copyright   : (c) Mesabloo and contributors, 2021-
-- License     : BSD3
-- Stability   : experimental
-- Portability : Portable
module Error.Diagnose.Report
  ( -- * Re-exports
    module Export,
  )
where

import Error.Diagnose.Report.Internal as Export (Marker (..), MarkerKind (..), Note (..), Report (..), Severity (..), errorToWarning, warningToError)
