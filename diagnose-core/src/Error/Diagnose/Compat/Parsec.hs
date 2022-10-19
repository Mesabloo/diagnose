{-# OPTIONS -Wno-name-shadowing #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Error.Diagnose.Compat.Parsec
-- Description : Compatibility layer for parsec
-- Copyright   : (c) Mesabloo, 2021-2022
-- License     : BSD3
-- Stability   : experimental
-- Portability : Portable
module Error.Diagnose.Compat.Parsec
  ( diagnosticFromParseError,
    errorDiagnosticFromParseError,
    warningDiagnosticFromParseError,
    module Error.Diagnose.Compat.Hints,
  )
where

import Data.Bifunctor (second)
import Data.List (intercalate, nub)
import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import Data.Void (Void)
import Error.Diagnose
import Error.Diagnose.Compat.Hints (HasHints (..))
import qualified Text.Parsec.Error as PE
import qualified Text.Parsec.Pos as PP

-- | Generates a diagnostic from a 'PE.ParseError'.
diagnosticFromParseError ::
  forall msg.
  (IsString msg, HasHints Void msg) =>
  -- | Determine whether the diagnostic is an error or a warning
  (PE.ParseError -> Severity) ->
  -- | An optional error code
  Maybe msg ->
  -- | The main error of the diagnostic
  msg ->
  -- | Default hints
  Maybe [Note msg] ->
  -- | The 'PE.ParseError' to transform into a 'Diagnostic'
  PE.ParseError ->
  Diagnostic msg
diagnosticFromParseError sev code msg (fromMaybe [] -> defaultHints) error =
  let pos = fromSourcePos $ PE.errorPos error
      markers = toMarkers pos $ PE.errorMessages error
      report = Report (sev error) code msg markers (defaultHints <> hints (undefined :: Void))
   in addReport def report
  where
    fromSourcePos :: PP.SourcePos -> SourceRange
    fromSourcePos pos =
      let start = both fromIntegral (PP.sourceLine pos, PP.sourceColumn pos)
          end = second (+ 1) start
       in Range start end (PP.sourceName pos)

    toMarkers :: SourceRange -> [PE.Message] -> [Marker msg 'MainMarker]
    toMarkers source [] = [Primary source . Just $ fromString "<<unknown error>>"]
    toMarkers source msgs =
      let putTogether [] = ([], [], [], [])
          putTogether (PE.SysUnExpect thing : ms) = let (a, b, c, d) = putTogether ms in (thing : a, b, c, d)
          putTogether (PE.UnExpect thing : ms) = let (a, b, c, d) = putTogether ms in (a, thing : b, c, d)
          putTogether (PE.Expect thing : ms) = let (a, b, c, d) = putTogether ms in (a, b, thing : c, d)
          putTogether (PE.Message thing : ms) = let (a, b, c, d) = putTogether ms in (a, b, c, thing : d)

          (nub -> sysUnexpectedList, nub -> unexpectedList, nub -> expectedList, nub -> messages) = putTogether msgs

          firstSysUnexpectedMessage = head sysUnexpectedList
          unexpectedMessage = "unexpected " <> if null unexpectedList then if null firstSysUnexpectedMessage then "end of line" else firstSysUnexpectedMessage else intercalate ", " (filter (not . null) unexpectedList)
       in [Primary source . Just $ fromString unexpectedMessage]
            <> [Primary source . Just $ fromString msg | msg <- messages]
            <> [Secondary source . Just . fromString $ "expecting any of " <> intercalate ", " (filter (not . null) expectedList)]

-- | Generates an error diagnostic from a 'PE.ParseError'.
errorDiagnosticFromParseError ::
  forall msg.
  (IsString msg, HasHints Void msg) =>
  -- | An optional error code
  Maybe msg ->
  -- | The main error message of the diagnostic
  msg ->
  -- | Default hints
  Maybe [Note msg] ->
  -- | The 'PE.ParseError' to convert
  PE.ParseError ->
  Diagnostic msg
errorDiagnosticFromParseError = diagnosticFromParseError (const Error)

-- | Generates a warning diagnostic from a 'PE.ParseError'.
warningDiagnosticFromParseError ::
  forall msg.
  (IsString msg, HasHints Void msg) =>
  -- | An optional error code
  Maybe msg ->
  -- | The main error message of the diagnostic
  msg ->
  -- | Default hints
  Maybe [Note msg] ->
  -- | The 'PE.ParseError' to convert
  PE.ParseError ->
  Diagnostic msg
warningDiagnosticFromParseError = diagnosticFromParseError (const Warning)

------------------------------------
------------ INTERNAL --------------
------------------------------------

-- | Applies a computation to both element of a tuple.
--
--   > both f = bimap @(,) f f
both :: (a -> b) -> (a, a) -> (b, b)
both f ~(x, y) = (f x, f y)
