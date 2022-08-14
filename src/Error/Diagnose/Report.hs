-- |
-- Module      : Error.Diagnose.Report
-- Description : Report definition and pretty printing
-- Copyright   : (c) Mesabloo, 2021-2022
-- License     : BSD3
-- Stability   : experimental
-- Portability : Portable
module Error.Diagnose.Report
  ( -- * Re-exports
    module Export,
  )
where

import Error.Diagnose.Report.Internal as Export (Marker (..), Note (..), Report(Warn, Err), err, errorToWarning, warn, warningToError)
