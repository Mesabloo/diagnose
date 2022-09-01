{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS -Wno-name-shadowing #-}

-- |
-- Module      : Error.Diagnose.Report.Internal
-- Description : Internal workings for report definitions and pretty printing.
-- Copyright   : (c) Mesabloo, 2021-2022
-- License     : BSD3
-- Stability   : experimental
-- Portability : Portable
--
-- /Warning/: The API of this module can break between two releases, therefore you should not rely on it.
--            It is also highly undocumented.
--
--            Please limit yourself to the "Error.Diagnose.Report" module, which exports some of the useful functions defined here.
module Error.Diagnose.Report.Internal
  ( module Error.Diagnose.Report.Internal,
    Report (.., Warn, Err),
  )
where

#ifdef USE_AESON
import Data.Aeson (ToJSON(..), object, (.=))
#endif
import Control.Applicative ((<|>))
import Data.Array.Unboxed (Array)
import Data.HashMap.Lazy (HashMap)
import Data.String (IsString (fromString))
import Error.Diagnose.Position

-- | A 'HashMap' associating a 'FilePath' to an array of lines indexed by line numbers.
type FileMap = HashMap FilePath (Array Int String)

-- | The type of diagnostic reports with abstract message type.
data Report msg
  = Report
      Bool
      -- ^ Is the report a warning or an error?
      (Maybe msg)
      -- ^ An optional error code to print at the top.
      msg
      -- ^ The message associated with the error.
      [(Position, Marker msg)]
      -- ^ A map associating positions with marker to show under the source code.
      [Note msg]
      -- ^ A list of notes to add at the end of the report.

-- | Pattern synonym for a warning report.
pattern Warn :: Maybe msg -> msg -> [(Position, Marker msg)] -> [Note msg] -> Report msg
pattern Warn errCode msg reports notes = Report False errCode msg reports notes

-- | Pattern synonym for an error report.
pattern Err :: Maybe msg -> msg -> [(Position, Marker msg)] -> [Note msg] -> Report msg
pattern Err errCode msg reports notes = Report True errCode msg reports notes

{-# COMPLETE Warn, Err #-}

instance Semigroup msg => Semigroup (Report msg) where
  Report isError1 code1 msg1 pos1 hints1 <> Report isError2 code2 msg2 pos2 hints2 =
    Report (isError1 || isError2) (code1 <|> code2) (msg1 <> msg2) (pos1 <> pos2) (hints1 <> hints2)

instance Monoid msg => Monoid (Report msg) where
  mempty = Report False Nothing mempty mempty mempty

#ifdef USE_AESON
instance ToJSON msg => ToJSON (Report msg) where
  toJSON (Report isError code msg markers hints) =
    object [ "kind" .= (if isError then "error" else "warning" :: String)
           , "code" .= code
           , "message" .= msg
           , "markers" .= fmap showMarker markers
           , "hints" .= hints
           ]
    where
      showMarker (pos, marker) =
        object $ [ "position" .= pos ]
              <> case marker of
                   This m  -> [ "message" .= m
                              , "kind" .= ("this" :: String)
                              ]
                   Where m -> [ "message" .= m
                              , "kind" .= ("where" :: String)
                              ]
                   Maybe m -> [ "message" .= m
                              , "kind" .= ("maybe" :: String)
                              ]
                   Blank -> [ "kind" .= ("blank" :: String) ]
#endif

-- | The type of markers with abstract message type, shown under code lines.
data Marker msg
  = -- | A red or yellow marker under source code, marking important parts of the code.
    This msg
  | -- | A blue marker symbolizing additional information.
    Where msg
  | -- | A magenta marker to report potential fixes.
    Maybe msg
  | -- | An empty marker, whose sole purpose is to include a line of code in the report without markers under.
    Blank

instance Eq (Marker msg) where
  This _ == This _ = True
  Where _ == Where _ = True
  Maybe _ == Maybe _ = True
  Blank == Blank = True
  _ == _ = False
  {-# INLINEABLE (==) #-}

instance Ord (Marker msg) where
  This _ < _ = False
  Where _ < This _ = True
  Where _ < _ = False
  Maybe _ < _ = True
  _ < Blank = True
  Blank < _ = False
  {-# INLINEABLE (<) #-}

  m1 <= m2 = m1 < m2 || m1 == m2
  {-# INLINEABLE (<=) #-}

-- | A note is a piece of information that is found at the end of a report.
data Note msg
  = -- | A note, which is meant to give valuable information related to the encountered error.
    Note msg
  | -- | A hint, to propose potential fixes or help towards fixing the issue.
    Hint msg

#ifdef USE_AESON
instance ToJSON msg => ToJSON (Note msg) where
  toJSON (Note msg) = object [ "note" .= msg ]
  toJSON (Hint msg) = object [ "hint" .= msg ]
#endif

-- | Constructs a 'Note' from the given message as a literal string.
instance IsString msg => IsString (Note msg) where
  fromString = Note . fromString

-- | Constructs a warning or an error report.
warn,
  err ::
    -- | An optional error code to be shown right next to "error" or "warning".
    Maybe msg ->
    -- | The report message, shown at the very top.
    msg ->
    -- | A list associating positions with markers.
    [(Position, Marker msg)] ->
    -- | A possibly mempty list of hints to add at the end of the report.
    [Note msg] ->
    Report msg
warn = Report False
{-# INLINE warn #-}
{-# DEPRECATED warn "'warn' is deprecated. Use 'Warn' instead." #-}
err = Report True
{-# INLINE err #-}
{-# DEPRECATED err "'err' is deprecated. Use 'Err' instead." #-}

-- | Transforms a warning report into an error report.
warningToError :: Report msg -> Report msg
warningToError (Report False code msg markers notes) = Report True code msg markers notes
warningToError r@(Report True _ _ _ _) = r

-- | Transforms an error report into a warning report.
errorToWarning :: Report msg -> Report msg
errorToWarning (Report True code msg markers notes) = Report False code msg markers notes
errorToWarning r@(Report False _ _ _ _) = r
