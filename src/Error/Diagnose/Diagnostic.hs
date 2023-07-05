{-# LANGUAGE CPP #-}

-- |
-- Module      : Error.Diagnose.Diagnostic
-- Description : Diagnostic definition and pretty printing
-- Copyright   : (c) Mesabloo, 2021-2022
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
    errorsToWarnings,
    hasReports,
    reportsOf,
    prettyDiagnostic,
    prettyDiagnostic',
    printDiagnostic,
    printDiagnostic',
    warningsToErrors,
    WithUnicode(..),
    TabSize(..),
  )
import System.IO as Export (stderr, stdout)
