{-|
Module      : Error.Diagnose.Diagnostic
Description : Diagnostic definition and pretty printing
Copyright   : (c) Mesabloo, 2021
License     : BSD3
Stability   : experimental
Portability : Portable
-}
module Error.Diagnose.Diagnostic
  ( -- * Re-exports
    module Export ) where

import Error.Diagnose.Diagnostic.Internal as Export (Diagnostic, def, printDiagnostic, diagnosticToJson, addFile, addReport)

import System.IO as Export (stdout, stderr)
