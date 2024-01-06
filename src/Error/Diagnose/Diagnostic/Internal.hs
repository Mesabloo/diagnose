{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : Error.Diagnose.Diagnostic.Internal
-- Description : Internal workings for diagnostic definitions and pretty printing.
-- Copyright   : (c) Mesabloo, 2021-2022
-- License     : BSD3
-- Stability   : experimental
-- Portability : Portable
--
-- /Warning/: The API of this module can break between two releases, therefore you should not rely on it.
--            It is also highly undocumented.
--
--            Please limit yourself to the "Error.Diagnose.Diagnostic" module, which exports some of the useful functions defined here.
module Error.Diagnose.Diagnostic.Internal (module Error.Diagnose.Diagnostic.Internal, WithUnicode(..), TabSize(..)) where

import Control.Monad.IO.Class (MonadIO, liftIO)
#ifdef USE_AESON
import Data.Aeson (ToJSON(..), encode, object, (.=))
import Data.ByteString.Lazy (ByteString)
#endif

import Data.Array (listArray)
import Data.DList (DList)
import qualified Data.DList as DL
import Data.Foldable (fold, toList)
import Data.List (intersperse)
import Error.Diagnose.Report (Report)
import Error.Diagnose.Report.Internal (FileMap, errorToWarning, prettyReport, warningToError, WithUnicode(..), TabSize(..))
import Error.Diagnose.Style (Annotation, Style)
import Prettyprinter (Doc, Pretty, hardline, pretty, defaultLayoutOptions, reAnnotateS, layoutPretty)
import Prettyprinter.Render.Terminal (renderIO)
import System.IO (Handle)

-- | The data type for diagnostic containing messages of an abstract type.
--
--   Users can use 'mempty' to create a new empty diagnostic, and 'addFile' and
--   'addReport' to alter its internal state.
data Diagnostic msg
  = Diagnostic
      (DList (Report msg))
      -- ^ All the reports contained in a diagnostic.
      --
      --   Reports are output one by one, without connections in between.
      !FileMap
      -- ^ A map associating files with their content as lists of lines.
  deriving (Functor, Foldable, Traversable)

instance Monoid (Diagnostic msg) where
  mempty = Diagnostic mempty mempty

instance Semigroup (Diagnostic msg) where
  Diagnostic rs1 files1 <> Diagnostic rs2 files2 = Diagnostic (rs1 <> rs2) (files1 <> files2)

#ifdef USE_AESON
instance ToJSON msg => ToJSON (Diagnostic msg) where
  toJSON (Diagnostic reports files) =
    object [ "files" .= fmap toJSONFile (fmap toList <$> files)
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

-- | Transforms every warning report in this diagnostic into an error report.
warningsToErrors :: Diagnostic msg -> Diagnostic msg
warningsToErrors (Diagnostic reports files) = Diagnostic (warningToError <$> reports) files

-- | Transforms every error report in this diagnostic into a warning report.
errorsToWarnings :: Diagnostic msg -> Diagnostic msg
errorsToWarnings (Diagnostic reports files) = Diagnostic (errorToWarning <$> reports) files

-- | Pretty prints a 'Diagnostic' into a 'Doc'ument that can be output using 'hPutDoc'.
--
--   Colors are put by default.
--   If you do not want these, just 'unAnnotate' the resulting document like so:
--
--   >>> let doc = unAnnotate (prettyDiagnostic withUnicode tabSize diagnostic)
--
--   Changing the style is also rather easy:
--
--   >>> let myCustomStyle :: Style = _
--   >>> let doc = myCustomStyle (prettyDiagnostic withUnicode tabSize diagnostic)
prettyDiagnostic ::
  Pretty msg =>
  -- | Should we use unicode when printing paths?
  WithUnicode ->
  -- | The number of spaces each TAB character will span.
  TabSize ->
  -- | The diagnostic to print.
  Diagnostic msg ->
  Doc (Annotation ann)
prettyDiagnostic withUnicode tabSize =
  prettyDiagnostic' withUnicode tabSize . fmap pretty
{-# INLINE prettyDiagnostic #-}

-- | Like 'prettyDiagnostic' except that instead of requiring a 'pretty'
-- instance for messages, this allows passing in your own 'Doc'. Custom
-- annotations are retained in 'OtherStyle'
prettyDiagnostic' ::
  -- | Should we use unicode when printing paths?
  WithUnicode ->
  -- | The number of spaces each TAB character will span.
  TabSize ->
  -- | The diagnostic to print.
  Diagnostic (Doc ann) ->
  Doc (Annotation ann)
prettyDiagnostic' withUnicode tabSize (Diagnostic reports file) =
  fold . intersperse hardline $ prettyReport file withUnicode tabSize <$> toList reports

-- | Prints a 'Diagnostic' onto a specific 'Handle'.
printDiagnostic ::
  (MonadIO m, Pretty msg) =>
  -- | The handle onto which to output the diagnostic.
  Handle ->
  -- | Should we print with unicode characters?
  WithUnicode ->
  -- | The number of spaces each TAB character will span.
  TabSize ->
  -- | The style in which to output the diagnostic.
  Style ann ->
  -- | The diagnostic to output.
  Diagnostic msg ->
  m ()
printDiagnostic handle withUnicode tabSize style =
  printDiagnostic' handle withUnicode tabSize style . fmap pretty
{-# INLINE printDiagnostic #-}

-- | Like 'printDiagnostic' except that instead of requiring a 'pretty'
-- instance for messages, this allows passing in your own 'Doc'.
printDiagnostic' ::
  MonadIO m =>
  -- | The handle onto which to output the diagnostic.
  Handle ->
  -- | Should we print with unicode characters?
  WithUnicode ->
  -- | The number of spaces each TAB character will span.
  TabSize ->
  -- | The style in which to output the diagnostic.
  Style ann ->
  -- | The diagnostic to output.
  Diagnostic (Doc ann) ->
  m ()
printDiagnostic' handle withUnicode tabSize style =
  liftIO
    . renderIO handle
    . reAnnotateS style
    . layoutPretty defaultLayoutOptions
    . prettyDiagnostic' withUnicode tabSize

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
   in Diagnostic reports ((path, lineArray) : files)
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
-- | Creates a JSON object from a diagnostic, containing those fields (only types are indicated):
--
--   > { files:
--   >     { name: string
--   >     , content: string[]
--   >     }[]
--   > , reports:
--   >     { kind: 'error' | 'warning'
--   >     , code: T?
--   >     , message: T
--   >     , markers:
--   >         { kind: 'this' | 'where' | 'maybe'
--   >         , position:
--   >             { beginning: { line: int, column: int }
--   >             , end: { line: int, column: int }
--   >             , file: string
--   >             }
--   >         , message: T
--   >         }[]
--   >     , hints: ({ note: T } | { hint: T })[]
--   >     }[]
--   > }
--
--   where @T@ is the type of the JSON representation for the @msg@ type variable.
diagnosticToJson :: ToJSON msg => Diagnostic msg -> ByteString
diagnosticToJson = encode
#endif
