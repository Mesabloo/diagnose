{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS -Wno-name-shadowing #-}

-- |
-- Module      : Error.Diagnose.Compat.Megaparsec
-- Description : Compatibility layer for megaparsec
-- Copyright   : (c) Mesabloo, 2021-2022
-- License     : BSD3
-- Stability   : experimental
-- Portability : Portable
module Error.Diagnose.Compat.Megaparsec
  ( diagnosticFromBundle,
    errorDiagnosticFromBundle,
    warningDiagnosticFromBundle,
    module Error.Diagnose.Compat.Hints,
  )
where

import Data.Bifunctor (second)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set (toList)
import Data.String (IsString (..))
import Error.Diagnose
import Error.Diagnose.Compat.Hints (HasHints (..))
import qualified Text.Megaparsec as MP

-- | Transforms a megaparsec 'MP.ParseErrorBundle' into a well-formated 'Diagnostic' ready to be shown.
diagnosticFromBundle ::
  forall msg s e.
  (IsString msg, MP.Stream s, HasHints e msg, MP.ShowErrorComponent e, MP.VisualStream s, MP.TraversableStream s) =>
  -- | How to decide whether this is an error or a warning diagnostic
  (MP.ParseError s e -> Severity) ->
  -- | An optional error code
  Maybe msg ->
  -- | The error message of the diagnostic
  msg ->
  -- | Default hints when trivial errors are reported
  Maybe [Note msg] ->
  -- | The bundle to create a diagnostic from
  MP.ParseErrorBundle s e ->
  Diagnostic msg
diagnosticFromBundle sev code msg (fromMaybe [] -> trivialHints) MP.ParseErrorBundle {..} =
  foldl addReport def (toLabeledPosition <$> bundleErrors)
  where
    toLabeledPosition :: MP.ParseError s e -> Report msg
    toLabeledPosition error =
      let (_, pos) = MP.reachOffset (MP.errorOffset error) bundlePosState
          source = fromSourcePos (MP.pstateSourcePos pos)
          msgs = fromString @msg <$> lines (MP.parseErrorTextPretty error)
       in flip
            (Report (sev error) code msg)
            (errorHints error)
            if
                | [m] <- msgs -> [Primary source (Just m)]
                | [m1, m2] <- msgs -> [Primary source (Just m1), Secondary source (Just m2)]
                | otherwise -> [Primary source . Just $ fromString "<<Unknown error>>"]

    fromSourcePos :: MP.SourcePos -> SourceRange
    fromSourcePos MP.SourcePos {..} =
      let start = both (fromIntegral . MP.unPos) (sourceLine, sourceColumn)
          end = second (+ 1) start
       in Range start end sourceName

    errorHints :: MP.ParseError s e -> [Note msg]
    errorHints MP.TrivialError {} = trivialHints
    errorHints (MP.FancyError _ errs) =
      Set.toList errs >>= \case
        MP.ErrorCustom e -> hints e
        _ -> mempty

-- | Creates an error diagnostic from a megaparsec 'MP.ParseErrorBundle'.
errorDiagnosticFromBundle ::
  forall msg s e.
  (IsString msg, MP.Stream s, HasHints e msg, MP.ShowErrorComponent e, MP.VisualStream s, MP.TraversableStream s) =>
  -- | An optional error code
  Maybe msg ->
  -- | The error message of the diagnostic
  msg ->
  -- | Default hints when trivial errors are reported
  Maybe [Note msg] ->
  -- | The bundle to create a diagnostic from
  MP.ParseErrorBundle s e ->
  Diagnostic msg
errorDiagnosticFromBundle = diagnosticFromBundle (const Error)

-- | Creates a warning diagnostic from a megaparsec 'MP.ParseErrorBundle'.
warningDiagnosticFromBundle ::
  forall msg s e.
  (IsString msg, MP.Stream s, HasHints e msg, MP.ShowErrorComponent e, MP.VisualStream s, MP.TraversableStream s) =>
  -- | An optional error code
  Maybe msg ->
  -- | The error message of the diagnostic
  msg ->
  -- | Default hints when trivial errors are reported
  Maybe [Note msg] ->
  -- | The bundle to create a diagnostic from
  MP.ParseErrorBundle s e ->
  Diagnostic msg
warningDiagnosticFromBundle = diagnosticFromBundle (const Warning)

------------------------------------
------------ INTERNAL --------------
------------------------------------

-- | Applies a computation to both element of a tuple.
--
--   > both f = bimap @(,) f f
both :: (a -> b) -> (a, a) -> (b, b)
both f ~(x, y) = (f x, f y)
