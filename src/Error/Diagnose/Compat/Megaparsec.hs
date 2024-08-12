{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
--
-- This may be accompanied by providing a specific instance of 'HasHints' for the
-- specific user error type used with megaparsec (i.e. the @e@ in @Parser e s@).
-- If no specific instance is provided, a default error report with no hints is
-- generated.
diagnosticFromBundle ::
  forall msg s e.
  (IsString msg, MP.Stream s, HasHints e msg, MP.ShowErrorComponent e, MP.VisualStream s, MP.TraversableStream s
  , AddBundleError e msg ) =>
  -- | How to decide whether this is an error or a warning diagnostic
  (MP.ParseError s e -> Bool) ->
  -- | An optional error code
  Maybe msg ->
  -- | The error message of the diagnostic
  msg ->
  -- | Default hints when trivial errors are reported
  Maybe [Note msg] ->
  -- | The bundle to create a diagnostic from
  MP.ParseErrorBundle s e ->
  Diagnostic msg
diagnosticFromBundle isError code msg (fromMaybe [] -> trivialHints) MP.ParseErrorBundle {..} =
  foldl (addBundleError toLabeledPosition) mempty bundleErrors
  where
    toLabeledPosition :: MP.ParseError s e -> [Report msg]
    toLabeledPosition error =
      let (_, pos) = MP.reachOffset (MP.errorOffset error) bundlePosState
          source = fromSourcePos (MP.pstateSourcePos pos)
          msgs = fromString @msg <$> lines (MP.parseErrorTextPretty error)
          errRep = flip
            (if isError error then Err code msg else Warn code msg)
            (errorHints error)
            if
                | [m] <- msgs -> [(source, This m)]
                | [m1, m2] <- msgs -> [(source, This m1), (source, Where m2)]
                | otherwise -> [(source, This $ fromString "<<Unknown error>>")]
      in case error of
        MP.TrivialError {} -> [errRep]
        MP.FancyError _ errs ->
          let mkRep = \case
                MP.ErrorCustom ce -> mkReports [errRep] source ce
                _ -> [errRep]
          in concat $ mkRep <$> Set.toList errs

    fromSourcePos :: MP.SourcePos -> Position
    fromSourcePos MP.SourcePos {..} =
      let start = both (fromIntegral . MP.unPos) (sourceLine, sourceColumn)
          end = second (+ 1) start
       in Position start end sourceName

    errorHints :: MP.ParseError s e -> [Note msg]
    errorHints MP.TrivialError {} = trivialHints
    errorHints (MP.FancyError _ errs) =
      Set.toList errs >>= \case
        MP.ErrorCustom e -> hints e
        _ -> mempty

class AddBundleError e msg where
  addBundleError :: (MP.ParseError s e -> [Report msg])
                 -> Diagnostic msg
                 -> MP.ParseError s e
                 -> Diagnostic msg

instance {-# OVERLAPPING #-} AddBundleError (Diagnostic msg) msg where
  addBundleError defaultFun diag err =
    case err of
      MP.FancyError _ errs ->
        let eachErr = \case
              MP.ErrorCustom d -> (d <>)
              _ -> flip (foldl addReport) (defaultFun err)
        in foldr eachErr diag (Set.toList errs)
      _ -> foldl addReport diag (defaultFun err)

instance {-# OVERLAPPABLE #-} AddBundleError e msg where
  addBundleError defaultFun diag = foldl addReport diag . defaultFun


-- | Creates an error diagnostic from a megaparsec 'MP.ParseErrorBundle'.
errorDiagnosticFromBundle ::
  forall msg s e.
  (IsString msg, MP.Stream s, HasHints e msg, MP.ShowErrorComponent e, MP.VisualStream s, MP.TraversableStream s
  , AddBundleError e msg ) =>
  -- | An optional error code
  Maybe msg ->
  -- | The error message of the diagnostic
  msg ->
  -- | Default hints when trivial errors are reported
  Maybe [Note msg] ->
  -- | The bundle to create a diagnostic from
  MP.ParseErrorBundle s e ->
  Diagnostic msg
errorDiagnosticFromBundle = diagnosticFromBundle (const True)

-- | Creates a warning diagnostic from a megaparsec 'MP.ParseErrorBundle'.
warningDiagnosticFromBundle ::
  forall msg s e.
  (IsString msg, MP.Stream s, HasHints e msg, MP.ShowErrorComponent e, MP.VisualStream s, MP.TraversableStream s
  , AddBundleError e msg ) =>
  -- | An optional error code
  Maybe msg ->
  -- | The error message of the diagnostic
  msg ->
  -- | Default hints when trivial errors are reported
  Maybe [Note msg] ->
  -- | The bundle to create a diagnostic from
  MP.ParseErrorBundle s e ->
  Diagnostic msg
warningDiagnosticFromBundle = diagnosticFromBundle (const False)

------------------------------------
------------ INTERNAL --------------
------------------------------------

-- | Applies a computation to both element of a tuple.
--
--   > both f = bimap @(,) f f
both :: (a -> b) -> (a, a) -> (b, b)
both f ~(x, y) = (f x, f y)
