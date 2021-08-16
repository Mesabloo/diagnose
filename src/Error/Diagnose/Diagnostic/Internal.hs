{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : Error.Diagnose.Diagnostic.Internal
Description : Internal workings for diagnostic definitions and pretty printing.
Copyright   : (c) Mesabloo, 2021
License     : BSD3
Stability   : experimental
Portability : Portable

/Warning/: The API of this module can break between two releases, therefore you should not rely on it.
           It is also highly undocumented.

           Please limit yourself to the "Error.Diagnose.Diagnostic" module, which exports some of the useful functions defined here.
-}
module Error.Diagnose.Diagnostic.Internal (module Error.Diagnose.Diagnostic.Internal, def) where

import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Aeson (ToJSON(..), encode, object, (.=))
import Data.ByteString.Lazy (ByteString)
import Data.Default (Default, def)
import Data.Foldable (fold)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.List (intersperse)

import Error.Diagnose.Report (Report)
import Error.Diagnose.Report.Internal (prettyReport)

import System.IO (Handle)

import Text.PrettyPrint.ANSI.Leijen (Pretty, Doc, hardline, hPutDoc, plain)


-- | The data type for diagnostic containing messages of an abstract type.
--
--   The constructors are private, but users can use 'def' from the 'Default' typeclass
--   to create a new empty diagnostic, and 'addFile' and 'addReport' to alter its internal state.
data Diagnostic msg
  = Diagnostic
      [Report msg]                  -- ^ All the reports contained in a diagnostic.
                                    --
                                    --   Reports are output one by one, without connections in between.
      (HashMap FilePath [String])   -- ^ A map associating files with their content as lists of lines.

instance Default (Diagnostic msg) where
  def = Diagnostic mempty mempty

instance Semigroup (Diagnostic msg) where
  Diagnostic rs1 file <> Diagnostic rs2 _ = Diagnostic (rs1 <> rs2) file

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

-- | Pretty prints a diagnostic into a 'Doc'ument that can be output using
--   'Text.PrettyPrint.ANSI.Leijen.hPutDoc' or 'Text.PrettyPrint.ANSI.Leijen.displayIO'.
prettyDiagnostic :: Pretty msg
                 => Bool                -- ^ Should we use unicode when printing paths?
                 -> Diagnostic msg      -- ^ The diagnostic to print
                 -> Doc
prettyDiagnostic withUnicode (Diagnostic reports file) =
  fold . intersperse hardline $ prettyReport file withUnicode <$> reports
{-# INLINE prettyDiagnostic #-}

-- | Prints a 'Diagnostic' onto a specific 'Handle'.
printDiagnostic :: (MonadIO m, Pretty msg)
                => Handle                   -- ^ The handle onto which to output the diagnostic.
                -> Bool                     -- ^ Should we print with unicode characters?
                -> Bool                     -- ^ 'False' to disable colors.
                -> Diagnostic msg           -- ^ The diagnostic to output.
                -> m ()
printDiagnostic handle withUnicode withColors diag =
  liftIO $ hPutDoc handle (unlessId withColors plain $ prettyDiagnostic withUnicode diag)
  where
    unlessId cond app = if cond then id else app
    {-# INLINE unlessId #-}
{-# INLINE printDiagnostic #-}

-- | Inserts a new referenceable file within the diagnostic.
addFile :: Diagnostic msg
        -> FilePath         -- ^ The path to the file.
        -> String           -- ^ The content of the file as a single string, where lines are ended by @\n@.
        -> Diagnostic msg
addFile (Diagnostic reports files) path content =
  Diagnostic reports (HashMap.insert path (lines content) files)

-- | Inserts a new report into a diagnostic.
addReport :: Diagnostic msg
          -> Report msg       -- ^ The new report to add to the diagnostic.
          -> Diagnostic msg
addReport (Diagnostic reports files) report =
  Diagnostic (report : reports) files

-- | Creates a JSON object from a diagnostic, containing those fields (only types are indicated):
--
--   > { files:
--   >     { name: string
--   >     , content: string[]
--   >     }[]
--   > , reports:
--   >     { kind: 'error' | 'warning'
--   >     , message: string
--   >     , markers:
--   >         { kind: 'this' | 'where' | 'maybe'
--   >         , position:
--   >             { beginning: { line: int, column: int }
--   >             , end: { line: int, column: int }
--   >             , file: string
--   >             }
--   >         , message: string
--   >         }[]
--   >     , hints: string[]
--   >     }[]
--   > }
diagnosticToJson :: ToJSON msg => Diagnostic msg -> ByteString
diagnosticToJson = encode
