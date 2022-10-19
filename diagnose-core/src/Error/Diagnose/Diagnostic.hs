{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}

-- |
-- Module      : Error.Diagnose.Diagnostic
-- Description : Diagnostic definition and pretty printing.
-- Copyright   : (c) Mesabloo and contributors, 2021-
-- License     : BSD3
-- Stability   : experimental
-- Portability : Portable
module Error.Diagnose.Diagnostic
  ( -- * Re-exports
    module Export,
  )
where

import Error.Diagnose.Diagnostic.Internal as Export
  ( Diagnostic,
#ifdef USE_AESON
    diagnosticToJson,
#endif
    addFile,
    addReport,
    def,
    errorsToWarnings,
    filesOf,
    hasReports,
    printDiagnostic,
    reportsOf,
    warningsToErrors,
  )
import System.IO as Export (stderr, stdout)
