{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS -Wno-name-shadowing #-}

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
  (PE.ParseError -> Bool) ->
  -- | An optional error code
  Maybe msg ->
  -- | The main error of the diagnostic
  msg ->
  -- | Default hints
  Maybe [Note msg] ->
  -- | The 'PE.ParseError' to transform into a 'Diagnostic'
  PE.ParseError ->
  Diagnostic msg
diagnosticFromParseError isError code msg (fromMaybe [] -> defaultHints) error =
  let pos = fromSourcePos $ PE.errorPos error
      markers = toMarkers pos $ PE.errorMessages error
      report = (if isError error then Err code msg else Warn code msg) markers (defaultHints <> hints (undefined :: Void))
   in addReport mempty report
  where
    fromSourcePos :: PP.SourcePos -> Position
    fromSourcePos pos =
      let start = both fromIntegral (PP.sourceLine pos, PP.sourceColumn pos)
          end = second (+ 1) start
       in Position start end (PP.sourceName pos)

    toMarkers :: Position -> [PE.Message] -> [(Position, Marker msg)]
    toMarkers source [] = [(source, This $ fromString "<<unknown error>>")]
    toMarkers source msgs =
      let putTogether [] = ([], [], [], [])
          putTogether (PE.SysUnExpect thing : ms) = let (a, b, c, d) = putTogether ms in (thing : a, b, c, d)
          putTogether (PE.UnExpect thing : ms) = let (a, b, c, d) = putTogether ms in (a, thing : b, c, d)
          putTogether (PE.Expect thing : ms) = let (a, b, c, d) = putTogether ms in (a, b, thing : c, d)
          putTogether (PE.Message thing : ms) = let (a, b, c, d) = putTogether ms in (a, b, c, thing : d)

          (nub -> sysUnexpectedList, nub -> unexpectedList, nub -> expectedList, nub -> messages) = putTogether msgs

          firstSysUnexpectedMessage = head sysUnexpectedList
          unexpectedMessage = "unexpected " <> if null unexpectedList then if null firstSysUnexpectedMessage then "end of line" else firstSysUnexpectedMessage else intercalate ", " (filter (not . null) unexpectedList)
       in [ (source, This $ fromString unexpectedMessage) ]
            <> [ (source, This $ fromString msg) | msg <- messages ]
            <> [ (source, Where $ fromString $ "expecting any of " <> intercalate ", " (filter (not . null) expectedList)) ]

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
errorDiagnosticFromParseError = diagnosticFromParseError (const True)

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
warningDiagnosticFromParseError = diagnosticFromParseError (const False)

------------------------------------
------------ INTERNAL --------------
------------------------------------

-- | Applies a computation to both element of a tuple.
--
--   > both f = bimap @(,) f f
both :: (a -> b) -> (a, a) -> (b, b)
both f ~(x, y) = (f x, f y)
