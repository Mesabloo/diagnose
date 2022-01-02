{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

import Error.Diagnose
    ( printDiagnostic,
#ifdef USE_AESON
      diagnosticToJson,
#endif
      stdout,
      err,
      warn,
      Marker(..),
      Report,
      Position(..),
      addFile,
      addReport,
      def )

#ifdef USE_AESON
import qualified Data.ByteString.Lazy as BS
#endif
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap

import System.IO (hPutStrLn)

main :: IO ()
main = do
  let files :: HashMap FilePath String = HashMap.fromList
        [ ("test.zc", "let id<a>(x : a) : a := x + 1\nrec fix(f) := f(fix(f))\nlet const<a, b>(x : a, y : b) : a := x")
        , ("somefile.zc", "let id<a>(x : a) : a := x\n  + 1")
        ]

  let reports =
        [ errorNoMarkersNoHints
        , errorSingleMarkerNoHints
        , warningSingleMarkerNoHints
        , errorTwoMarkersSameLineNoOverlapNoHints
        , errorSingleMarkerOutOfBoundsNoHints
        , errorTwoMarkersSameLineOverlapNoHints
        , errorTwoMarkersSameLinePartialOverlapNoHints
        , errorTwoMarkersTwoLinesNoHints
        , realWorldExample
        , errorTwoMarkersSamePositionNoHints
        , errorThreeMarkersWithOverlapNoHints
        , errorWithMultilineErrorNoMarkerNoHints
        , errorSingleMultilineMarkerMessageNoHints
        , errorTwoMarkersSameOriginOverlapNoHints
        , errorNoMarkersSingleHint
        , errorNoMarkersSingleMultilineHint
        , errorNoMarkersTwoHints
        , errorSingleMultilineMarkerNoHints
        , errorTwoMarkersWithMultilineNoHints
        , errorTwoMultilineMarkersNoHints
        , errorSingleMultilineMarkerMultilineMessageNoHints
        , errorTwoMultilineMarkersFirstMultilineMessageNoHints
        , errorThreeMultilineMarkersTwoMultilineMessageNoHints
        , errorOrderSensitive
        , beautifulExample 
        ] 

  let diag = HashMap.foldlWithKey' addFile (foldr (flip addReport) def reports) files

  hPutStrLn stdout "\n\nWith unicode: ─────────────────────────\n"
  printDiagnostic stdout True True diag
  hPutStrLn stdout "\n\nWithout unicode: ----------------------\n"
  printDiagnostic stdout False True diag
#ifdef USE_AESON
  hPutStrLn stdout "\n\nAs JSON: ------------------------------\n"
  BS.hPutStr stdout (diagnosticToJson diag)
#endif
  hPutStrLn stdout "\n"

errorNoMarkersNoHints :: Report String
errorNoMarkersNoHints =
  err "Error with no marker"
    []
    []

errorSingleMarkerNoHints :: Report String
errorSingleMarkerNoHints =
  err "Error with one marker in bounds"
    [ (Position (1, 25) (1, 30) "test.zc", This "Required here") ]
    []

warningSingleMarkerNoHints :: Report String
warningSingleMarkerNoHints =
  warn "Warning with one marker in bounds"
    [ (Position (1, 25) (1, 30) "test.zc", This "Required here") ]
    []

errorTwoMarkersSameLineNoOverlapNoHints :: Report String
errorTwoMarkersSameLineNoOverlapNoHints =
  err "Error with two markers in bounds (no overlap) on the same line"
    [ (Position (1, 5) (1, 10) "test.zc", This "First")
    , (Position (1, 15) (1, 22) "test.zc", Where "Second") ]
    []

errorSingleMarkerOutOfBoundsNoHints :: Report String
errorSingleMarkerOutOfBoundsNoHints =
  err "Error with one marker out of bounds"
    [ (Position (10, 5) (10, 15) "test2.zc", This "Out of bounds") ]
    []

errorTwoMarkersSameLineOverlapNoHints :: Report String
errorTwoMarkersSameLineOverlapNoHints =
  err "Error with two overlapping markers in bounds"
    [ (Position (1, 6) (1, 13) "test.zc", This "First")
    , (Position (1, 10) (1, 15) "test.zc", Where "Second") ]
    []

errorTwoMarkersSameLinePartialOverlapNoHints :: Report String
errorTwoMarkersSameLinePartialOverlapNoHints =
  err "Error with two partially overlapping markers in bounds"
    [ (Position (1, 5) (1, 25) "test.zc", This "First")
    , (Position (1, 12) (1, 20) "test.zc", Where "Second") ]
    []

errorTwoMarkersTwoLinesNoHints :: Report String
errorTwoMarkersTwoLinesNoHints =
  err "Error with two markers on two lines in bounds"
    [ (Position (1, 5) (1, 12) "test.zc", This "First")
    , (Position (2, 3) (2, 4) "test.zc", Where "Second") ]
    []

realWorldExample :: Report String
realWorldExample =
  err "Could not deduce constraint 'Num(a)' from the current context"
    [ (Position (1, 25) (1, 30) "test.zc", This "While applying function '+'")
    , (Position (1, 11) (1, 16) "test.zc", Where "'x' is supposed to have type 'a'")
    , (Position (1, 8) (1, 9) "test.zc", Where "type 'a' is bound here without constraints") ]
    [ "Adding 'Num(a)' to the list of constraints may solve this problem." ]

errorTwoMarkersSamePositionNoHints :: Report String
errorTwoMarkersSamePositionNoHints =
  err "Error with two markers on the same exact position in bounds"
    [ (Position (1, 6) (1, 10) "test.zc", This "First")
    , (Position (1, 6) (1, 10) "test.zc", Maybe "Second") ]
    []

errorThreeMarkersWithOverlapNoHints :: Report String
errorThreeMarkersWithOverlapNoHints =
  err "Error with three markers with overlapping in bounds"
    [ (Position (1, 9) (1, 15) "test.zc", This "First")
    , (Position (1, 9) (1, 18) "test.zc", Maybe "Second")
    , (Position (1, 6) (1, 10) "test.zc", Where "Third") ]
    []

errorWithMultilineErrorNoMarkerNoHints :: Report String
errorWithMultilineErrorNoMarkerNoHints =
  err "Error with multi\nline message and no markers"
    []
    []

errorSingleMultilineMarkerMessageNoHints :: Report String
errorSingleMultilineMarkerMessageNoHints =
  err "Error with single marker with multiline message"
    [ (Position (1, 9) (1, 15) "test.zc", This "First\nmultiline") ]
    []

errorTwoMarkersSameOriginOverlapNoHints :: Report String
errorTwoMarkersSameOriginOverlapNoHints =
  err "Error with two markers with same origin but partial overlap in bounds"
    [ (Position (1, 9) (1, 15) "test.zc", This "First")
    , (Position (1, 9) (1, 20) "test.zc", Maybe "Second") ]
    []

errorNoMarkersSingleHint :: Report String
errorNoMarkersSingleHint =
  err "Error with no marker and one hint"
    []
    [ "First hint" ]

errorNoMarkersSingleMultilineHint :: Report String
errorNoMarkersSingleMultilineHint =
  err "Error with no marker and one multiline hint"
    []
    [ "First multi\nline hint" ]

errorNoMarkersTwoHints :: Report String
errorNoMarkersTwoHints =
  err "Error with no markers and two hints"
    []
    [ "First hint"
    , "Second hint" ]

errorSingleMultilineMarkerNoHints :: Report String
errorSingleMultilineMarkerNoHints =
  err "Error with single marker spanning across multiple lines"
    [ (Position (1, 15) (2, 6) "test.zc", This "First") ]
    []

errorTwoMarkersWithMultilineNoHints :: Report String
errorTwoMarkersWithMultilineNoHints =
  err "Error with two markers, one single line and one multiline, in bounds"
    [ (Position (1, 9) (1, 13) "test.zc", This "First")
    , (Position (1, 14) (2, 6) "test.zc", Where "Second") ]
    []

errorTwoMultilineMarkersNoHints :: Report String
errorTwoMultilineMarkersNoHints =
  err "Error with two multiline markers in bounds"
    [ (Position (1, 9) (2, 5) "test.zc", This "First")
    , (Position (2, 1) (3, 10) "test.zc", Where "Second") ]
    []

errorSingleMultilineMarkerMultilineMessageNoHints :: Report String
errorSingleMultilineMarkerMultilineMessageNoHints =
  err "Error with one multiline marker with a multiline message in bounds"
    [ (Position (1, 9) (2, 5) "test.zc", This "Multi\nline message") ]
    []

errorTwoMultilineMarkersFirstMultilineMessageNoHints :: Report String
errorTwoMultilineMarkersFirstMultilineMessageNoHints =
  err "Error with two multiline markers with one multiline message in bounds"
    [ (Position (1, 9) (2, 5) "test.zc", This "First")
    , (Position (1, 9) (2, 6) "test.zc", Where "Multi\nline message") ]
    []

errorThreeMultilineMarkersTwoMultilineMessageNoHints :: Report String
errorThreeMultilineMarkersTwoMultilineMessageNoHints =
  err "Error with three multiline markers with two multiline messages in bounds"
    [ (Position (1, 9) (2, 5) "test.zc", This "First")
    , (Position (1, 9) (2, 6) "test.zc", Where "Multi\nline message")
    , (Position (1, 9) (2, 7) "test.zc", Maybe "Multi\nline message #2") ]
    []

errorOrderSensitive :: Report String
errorOrderSensitive =
  err "Order-sensitive labels with crossing"
    [ (Position (1, 1) (1, 7) "somefile.zc", This "Leftmost label")
    , (Position (1, 9) (1, 16) "somefile.zc", Where "Rightmost label") ]
    []

beautifulExample :: Report String
beautifulExample =
   err "Could not deduce constraint 'Num(a)' from the current context"
    [ (Position (1, 25) (2, 6) "somefile.zc", This "While applying function '+'")
    , (Position (1, 11) (1, 16) "somefile.zc", Where "'x' is supposed to have type 'a'")
    , (Position (1, 8) (1, 9) "somefile.zc", Where "type 'a' is bound here without constraints") ]
    [ "Adding 'Num(a)' to the list of constraints may solve this problem." ]
