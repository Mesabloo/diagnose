{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Module      : Error.Diagnose.Diagnostic.Internal
-- Description : Internal workings for diagnostic definitions and pretty printing.
-- Copyright   : (c) Mesabloo and contributors, 2021-
-- License     : BSD3
-- Stability   : experimental
-- Portability : Portable
--
-- /Warning/: The API of this module can break between two releases, therefore you should not rely on it.
--            It is also highly undocumented.
--
--            Please limit yourself to the "Error.Diagnose.Diagnostic" module, which exports some of the useful functions defined here.
module Error.Diagnose.Diagnostic.Internal (module Error.Diagnose.Diagnostic.Internal, def) where

import Control.Monad.IO.Class (MonadIO, liftIO)
#ifdef USE_AESON
import Data.Aeson (ToJSON(..), encode, object, (.=))
import Data.ByteString.Lazy (ByteString)
#endif

import Data.Array (listArray)
import Data.DList (DList)
import qualified Data.DList as DL
import Data.Default (Default, def)
import Data.Foldable (toList)
import qualified Data.HashMap.Lazy as HashMap
import Error.Diagnose.Pretty (Doc, Pretty, hPutDoc, unAnnotate, AnsiStyle)
import Error.Diagnose.Report (Report)
import Error.Diagnose.Report.Internal (FileMap, errorToWarning, warningToError)
import Error.Diagnose.Style (IsAnnotation, Style)
import System.IO (Handle)

-- | The data type for diagnostic containing messages of an abstract type.
--
--   The constructors are private, but users can use 'def' from the 'Default' typeclass
--   to create a new empty diagnostic, and 'addFile' and 'addReport' to alter its internal state.
data Diagnostic msg
  = Diagnostic
      (DList (Report msg))
      -- ^ All the reports contained in a diagnostic.
      --
      --   Reports are output one by one, without connections in between.
      !FileMap
      -- ^ A map associating files with their content as lists of lines.

instance Default (Diagnostic msg) where
  def = Diagnostic mempty mempty

instance Semigroup (Diagnostic msg) where
  Diagnostic rs1 file <> Diagnostic rs2 _ = Diagnostic (rs1 <> rs2) file

#ifdef USE_AESON
instance ToJSON msg => ToJSON (Diagnostic msg) where
  toJSON (Diagnostic reports files) =
    object [ "files" .= fmap toJSONFile (fmap toList <$> (HashMap.toList files))
           , "reports" .= reports
           ]
    where
      toJSONFile (path, content) =
        object [ "name" .= path
               , "content" .= content
               ]
#endif

-- | Checks whether the given diagnostic has any report or not (if it is effectively empty).
hasReports :: Diagnostic msg -> Bool
hasReports (Diagnostic DL.Nil _) = False
hasReports _ = True

-- | Retrieves the reports for this diagnostic.
reportsOf :: Diagnostic msg -> [Report msg]
reportsOf (Diagnostic reports _) = toList reports

-- | Retrieves all the files present inside the given diagnostic.
filesOf :: Diagnostic msg -> FileMap
filesOf (Diagnostic _ files) = files

-- | Transforms every warning report in this diagnostic into an error report.
warningsToErrors :: Diagnostic msg -> Diagnostic msg
warningsToErrors (Diagnostic reports files) = Diagnostic (warningToError <$> reports) files

-- | Transforms every error report in this diagnostic into a warning report.
errorsToWarnings :: Diagnostic msg -> Diagnostic msg
errorsToWarnings (Diagnostic reports files) = Diagnostic (errorToWarning <$> reports) files

-- | A type alias for layouting functions, where:
--
-- - The first argument tells whether to use Unicode characters or not
-- - The second argument is the number of spaces which must be used to render a @TAB@ character
-- - The third argument is the diagnostic to render
--
-- The resulting 'Doc'ument contains unprocessed style annotations.
type Layout msg ann = (IsAnnotation ann, Pretty msg) => Bool -> Int -> Diagnostic msg -> Doc ann

-- | Prints a 'Diagnostic' onto a specific 'Handle'.
printDiagnostic ::
  (MonadIO m, Pretty msg, IsAnnotation ann) =>
  -- | The handle onto which to output the diagnostic.
  Handle ->
  -- | Should we print with Unicode characters?
  Bool ->
  -- | 'False' to disable colors.
  Bool ->
  -- | The number of spaces each TAB character will span.
  Int ->
  -- | The style in which to output the diagnostic.
  Style ann ->
  -- | The layout to use to render the diagnostic.
  Layout msg ann ->
  -- | The diagnostic to output.
  Diagnostic msg ->
  m ()
printDiagnostic handle withUnicode withColors tabSize style layout diag =
  liftIO $ hPutDoc handle ((if withColors then style else unAnnotate) $ layout withUnicode tabSize diag)
{-# INLINE printDiagnostic #-}

-- | Pretty prints a 'Diagnostic' into a 'Doc'.
--
--   If colors are undesired, you can 'Prettyprinter.unAnnotate' the resulting 'Doc':
--
--   >>> unAnnotate (prettyDiagnostic True 4 myStyle myLayout myDiagnostic) :: Doc ann'
prettyDiagnostic ::
  (Pretty msg, IsAnnotation ann) =>
  -- | Whether to use Unicode characters or not.
  Bool ->
  -- | The size, in amount of spaces, to render TAB characters with.
  Int ->
  -- | The style to use for colors.
  Style ann ->
  -- | The layout used to render the error.
  Layout msg ann ->
  -- | The diagnostic to render.
  Diagnostic msg ->
  Doc AnsiStyle
prettyDiagnostic withUnicode tabSize style layout diag =
  style $ layout withUnicode tabSize diag
{-# INLINE prettyDiagnostic #-}

-- | Inserts a new referenceable file within the diagnostic.
addFile ::
  Diagnostic msg ->
  -- | The path to the file.
  FilePath ->
  -- | The content of the file as a single string, where lines are ended by @\\n@.
  String ->
  Diagnostic msg
addFile (Diagnostic reports files) path content =
  let fileLines = lines content
      lineCount = length fileLines
      lineArray = listArray (0, lineCount - 1) fileLines
   in Diagnostic reports (HashMap.insert path lineArray files)
{-# INLINE addFile #-}

-- | Inserts a new report into a diagnostic.
addReport ::
  Diagnostic msg ->
  -- | The new report to add to the diagnostic.
  Report msg ->
  Diagnostic msg
addReport (Diagnostic reports files) report =
  Diagnostic (reports `DL.snoc` report) files
{-# INLINE addReport #-}

#ifdef USE_AESON
-- | Creates a JSON object from a diagnostic, containing the fields underneath (only types are indicated).
--   @T@ stands for the @msg@ abstract type, and is put purely to symbolize where @msg@ is actually serialized.
--
--   > Position :
--   >   { "beginning": { line: int, column: int }
--   >   , "end": { line: int, column: int }
--   >   , "file": string
--   >   }
--
--   > Marker :
--   >     { "kind": "primary", "position": Position, "message": T? }
--   >   | { "kind": "secondary", "position": Position, "message": T? }
--   >   | { "kind": "blank", "position": Position }
--   >   | { "kind": "add", "position": Position, "code": string }
--   >   | { "kind": "remove", "position": Position }
--
--   > Note :
--   >   { "kind": "note" | "hint", "message": T, "markers": Marker? }
--
--   > Diagnostic :
--   >   { "files": [{ "name": string, "content": [string] }]
--   >   , "reports":
--   >     [{ "kind": "error" | "warning" | "critical"
--   >      , "code": T?
--   >      , "message": T
--   >      , "markers": [Marker]
--   >      , "notes": [Note]
--   >      }]
--   >   }
diagnosticToJson :: ToJSON msg => Diagnostic msg -> ByteString
diagnosticToJson = encode
#endif
