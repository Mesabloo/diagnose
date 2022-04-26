{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : Error.Diagnose.Diagnostic.Internal
-- Description : Internal workings for diagnostic definitions and pretty printing.
-- Copyright   : (c) Mesabloo, 2021
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
import Data.Default (Default, def)
import Data.Foldable (fold)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.List (intersperse)
import Error.Diagnose.Report (Report)
import Error.Diagnose.Report.Internal (prettyReport)
import Prettyprinter (Doc, Pretty, hardline, unAnnotate)
import Prettyprinter.Render.Terminal (AnsiStyle, hPutDoc)
import System.IO (Handle)

-- | The data type for diagnostic containing messages of an abstract type.
--
--   The constructors are private, but users can use 'def' from the 'Default' typeclass
--   to create a new empty diagnostic, and 'addFile' and 'addReport' to alter its internal state.
data Diagnostic msg
  = Diagnostic
      [Report msg]
      -- ^ All the reports contained in a diagnostic.
      --
      --   Reports are output one by one, without connections in between.
      (HashMap FilePath [String])
      -- ^ A map associating files with their content as lists of lines.

instance Default (Diagnostic msg) where
  def = Diagnostic mempty mempty

instance Semigroup (Diagnostic msg) where
  Diagnostic rs1 file <> Diagnostic rs2 _ = Diagnostic (rs1 <> rs2) file

#ifdef USE_AESON
instance ToJSON msg => ToJSON (Diagnostic msg) where
  toJSON (Diagnostic reports files) =
    object [ "files" .= fmap toJSONFile (HashMap.toList files)
           , "reports" .= reports
           ]
    where
      toJSONFile (path, content) =
        object [ "name" .= path
               , "content" .= content
               ]
#endif

-- | Pretty prints a diagnostic into a 'Doc'ument that can be output using
--   'Text.PrettyPrint.ANSI.Leijen.hPutDoc' or 'Text.PrettyPrint.ANSI.Leijen.displayIO'.
prettyDiagnostic ::
  Pretty msg =>
  -- | Should we use unicode when printing paths?
  Bool ->
  -- | The number of spaces each TAB character will span
  Int ->
  -- | The diagnostic to print
  Diagnostic msg ->
  Doc AnsiStyle
prettyDiagnostic withUnicode tabSize (Diagnostic reports file) =
  fold . intersperse hardline $ prettyReport file withUnicode tabSize <$> reports
{-# INLINE prettyDiagnostic #-}

-- | Prints a 'Diagnostic' onto a specific 'Handle'.
printDiagnostic ::
  (MonadIO m, Pretty msg) =>
  -- | The handle onto which to output the diagnostic.
  Handle ->
  -- | Should we print with unicode characters?
  Bool ->
  -- | 'False' to disable colors.
  Bool ->
  -- | The number of spaces each TAB character will span
  Int ->
  -- | The diagnostic to output.
  Diagnostic msg ->
  m ()
printDiagnostic handle withUnicode withColors tabSize diag =
  liftIO $ hPutDoc handle (unlessId withColors unAnnotate $ prettyDiagnostic withUnicode tabSize diag)
  where
    unlessId cond app = if cond then id else app
    {-# INLINE unlessId #-}
{-# INLINE printDiagnostic #-}

-- | Inserts a new referenceable file within the diagnostic.
addFile ::
  Diagnostic msg ->
  -- | The path to the file.
  FilePath ->
  -- | The content of the file as a single string, where lines are ended by @\n@.
  String ->
  Diagnostic msg
addFile (Diagnostic reports files) path content =
  Diagnostic reports (HashMap.insert path (lines content) files)
{-# INLINE addFile #-}

-- | Inserts a new report into a diagnostic.
addReport ::
  Diagnostic msg ->
  -- | The new report to add to the diagnostic.
  Report msg ->
  Diagnostic msg
addReport (Diagnostic reports files) report =
  Diagnostic (report : reports) files
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
--   >     , hints: T[]
--   >     }[]
--   > }
--
--   where @T@ is the type of the JSON representation for the @msg@ type variable.
diagnosticToJson :: ToJSON msg => Diagnostic msg -> ByteString
diagnosticToJson = encode
#endif
