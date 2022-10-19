{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS -Wno-name-shadowing #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Module      : Error.Diagnose.Report.Internal
-- Description : Internal workings for report definitions and pretty printing.
-- Copyright   : (c) Mesabloo and contributors, 2021-
-- License     : BSD3
-- Stability   : experimental
-- Portability : Portable
--
-- /Warning/: The API of this module can break between two releases, therefore you should not rely on it.
--            It is also highly undocumented.
--
--            Please limit yourself to the "Error.Diagnose.Report" module, which exports some of the useful functions defined here.
module Error.Diagnose.Report.Internal where

#ifdef USE_AESON
import Data.Aeson (ToJSON(..), object, (.=))
#endif
import Data.Array.Unboxed (Array)
import Data.Bifunctor (second)
import Data.HashMap.Lazy (HashMap)
import Data.Kind (Type)
import Error.Diagnose.Position (SourcePosition, SourceRange (..))

-- | A 'HashMap' associating a 'FilePath' to an array of lines indexed by line numbers.
type FileMap = HashMap FilePath (Array Int String)

-- | The type of diagnostic reports with abstract message type.
data Report msg
  = Report
      Severity
      -- ^ The severity of the report.
      (Maybe msg)
      -- ^ An optional error code to print with the report.
      msg
      -- ^ The message associated with the report.
      [Marker msg 'MainMarker]
      -- ^ A map associating positions to markers to show under the source code.
      [Note msg]
      -- ^ A list of notes and hints to show various help information after the error.

-- | The severity of a report describes how much it is important to take into account.
data Severity
  = -- | A warning report, which is important but not necessary to fix.
    Warning
  | -- | An error report, which stops the whole pipeline.
    Error
  | -- | A critical report, indicating that something internally failed.
    Critical

-- | The kind of a marker, which indicates where it can occur.
data MarkerKind
  = -- | A marker which occurs in the main section of the report.
    MainMarker
  | -- | A marker which occurs only in notes.
    NoteMarker

-- | The type of markers to be shown under source code.
data Marker msg :: MarkerKind -> Type where
  -- | A primary marker, which conveys the most important information (such as the location of an error).
  Primary :: !SourceRange -> Maybe msg -> Marker msg 'MainMarker
  -- | A secondary marker, which is meant to add extra information to the report.
  Secondary :: !SourceRange -> Maybe msg -> Marker msg 'MainMarker
  -- | A blank marker which is invisible but allows to include specific lines in the final report.
  Blank :: !SourceRange -> Marker msg 'MainMarker
  -- | We need a piece of code to be added inside the source code.
  AddCode ::
    -- | Where should we add the piece of code?
    {-# UNPACK #-} !SourcePosition ->
    -- | In which file should we add the piece of code?
    !FilePath ->
    {-# UNPACK #-} !Int ->
    -- | The piece of code to add to the original source code.
    String ->
    Marker msg 'NoteMarker
  -- | Code is requested to be deleted because it causes an error.
  RemoveCode :: !SourceRange -> Marker msg 'NoteMarker
  -- | A simple annotation under source code to add extra information in notes.
  Annotate :: !SourceRange -> Maybe msg -> Marker msg 'NoteMarker

#ifdef USE_AESON
instance ToJSON Severity where
  toJSON Warning = toJSON ("warning" :: String)
  toJSON Error = toJSON ("error" :: String)
  toJSON Critical = toJSON ("critical" :: String)

instance ToJSON msg => ToJSON (Report msg) where
  toJSON (Report sev code msg markers hints) =
    object [ "kind" .= sev
           , "code" .= code
           , "message" .= msg
           , "markers" .= markers
           , "notes" .= hints
           ]
    where

instance ToJSON msg => ToJSON (Marker msg k) where
  toJSON (Primary pos m) = object [ "message" .= m, "kind" .= ("primary" :: String), "position" .= pos ]
  toJSON (Secondary pos m) = object [ "message" .= m, "kind" .= ("secondary" :: String), "position" .= pos ]
  toJSON (Blank pos) = object [ "kind" .= ("blank" :: String), "position" .= pos ]
  toJSON (AddCode at file len code) = object [ "kind" .= ("add" :: String), "position" .= Range at (second (+ len) at) file, "code" .= code ]
  toJSON (RemoveCode pos) = object [ "kind" .= ("remove" :: String), "position" .= pos ]
  toJSON (Annotate pos m) = toJSON (Secondary pos m)
#endif

-- | A note is a piece of information that is found at the end of a report.
data Note msg
  = -- | A note, which is meant to give valuable information related to the encountered error.
    Note msg (Maybe (Marker msg 'NoteMarker))
  | -- | A hint, to propose potential fixes or help towards fixing the issue.
    Hint msg (Maybe (Marker msg 'NoteMarker))

#ifdef USE_AESON
instance ToJSON msg => ToJSON (Note msg) where
  toJSON (Note m marks) = object [ "kind" .= ("note" :: String), "message" .= m, "markers" .= marks ]
  toJSON (Hint m marks) = object [ "kind" .= ("hint" :: String), "message" .= m, "markers" .= marks ]
#endif

-- | Transforms a warning report into an error report.
warningToError :: Report msg -> Report msg
warningToError (Report Warning code msg markers notes) = Report Error code msg markers notes
warningToError r@(Report {}) = r

-- | Transforms an error report into a warning report.
errorToWarning :: Report msg -> Report msg
errorToWarning (Report Error code msg markers notes) = Report Warning code msg markers notes
errorToWarning r@(Report {}) = r
