{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

import qualified Data.ByteString.Lazy as BS
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Error.Diagnose
  ( Marker (..),
    Note (..),
    Report (..),
    Severity (..),
    SourceRange (..),
    addFile,
    addReport,
    def,
    diagnosticToJson,
    printDiagnostic,
    stdout,
  )
import Error.Diagnose.Layout.Ariadne (ariadneLayout)
import Error.Diagnose.Layout.Ariadne.Style (ariadneStyle)
import Error.Diagnose.Layout.GCC (gccLayout)
import Error.Diagnose.Layout.GCC.Style (gccStyle)
import Error.Diagnose.Layout.Typescript (typescriptLayout)
import Error.Diagnose.Layout.Typescript.Style (typescriptStyle)
import System.IO (hPutStrLn)

main :: IO ()
main = do
  let files :: HashMap FilePath String =
        HashMap.fromList
          [ ("test.zc", "let id<a>(x : a) : a := x + 1\nrec fix(f) := f(fix(f))\nlet const<a, b>(x : a, y : b) : a := x"),
            ("somefile.zc", "let id<a>(x : a) : a := x\n  + 1"),
            ("err.nst", "\n\n\n\n    = jmp g\n\n    g: forall(s: Ts, e: Tc).{ %r0: *s64 | s -> e }"),
            ("unsized.nst", "main: forall(a: Ta, s: Ts, e: Tc).{ %r5: forall().{| s -> e } | s -> %r5 }\n    = salloc a\n    ; sfree\n"),
            ("unicode.txt", "±⅀\t★♲♥🎉汉⑳⓴ჳᏁℳ爪"),
            ("gaps.txt", "abc\ndef\nghi\njkl\nmno\npqr"),
            ("repro3.file", "\n\n  ayo yoa\n    a b\n      c d e f g\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\nlayer qwertyy using yoooo: \"a\" \"b\" \"c\"\n"),
            ("sample.tao", "def five = match () in {\n    () => 5,\n    () => \"5\",\n}")
          ]

  let reports =
        [ errorNoMarkersNoHints,
          errorSingleMarkerNoHints,
          warningSingleMarkerNoHints,
          criticalSingleMarkerNoHints,
          errorTwoMarkersSameLineNoOverlapNoHints,
          errorSingleMarkerOutOfBoundsNoHints,
          errorTwoMarkersSameLineOverlapNoHints,
          errorTwoMarkersSameLinePartialOverlapNoHints,
          errorTwoMarkersTwoLinesNoHints,
          realWorldExample,
          errorTwoMarkersSameRangeNoHints,
          errorThreeMarkersWithOverlapNoHints,
          errorWithMultilineErrorNoMarkerNoHints,
          errorSingleMultilineMarkerMessageNoHints,
          errorTwoMarkersSameOriginOverlapNoHints,
          errorNoMarkersSingleHint,
          errorNoMarkersSingleMultilineHint,
          errorNoMarkersTwoHints,
          errorSingleMultilineMarkerNoHints,
          errorTwoMarkersWithMultilineNoHints,
          errorTwoMultilineMarkersNoHints,
          errorSingleMultilineMarkerMultilineMessageNoHints,
          errorTwoMultilineMarkersFirstMultilineMessageNoHints,
          errorThreeMultilineMarkersTwoMultilineMessageNoHints,
          errorOrderSensitive,
          errorMultilineAfterSingleLine,
          errorOnEmptyLine,
          errorMultipleFiles,
          errorWithCode,
          errorWithStrangeUnicodeInput,
          errorWithMultilineMarkerOn3Lines,
          errorMultilineMarkerNotAtEnd,
          errorWithLineGap,
          repro3,
          errorWithMultilineMarkerMessage,
          errorWithMultilineMarkerMessage',
          errorWithSingleBlankMarker,
          errorWithBlankAndNormalMarkerInLine,
          errorWithAnnotationMarkersInNotes,
          errorWithMultipleMarkersInNotes,
          errorWithMultilineCodeAdditionInNote,
          errorWithAMarkerOnFiveLines,
          ariadneReadmeExample,
          criticalFailureExample,
          beautifulExample
        ]

  let diag = HashMap.foldlWithKey' addFile (foldl addReport def reports) files

  hPutStrLn stdout "\n\nWith unicode: ─────────────────────────\n"
  printDiagnostic stdout True True 4 gccStyle gccLayout diag
  hPutStrLn stdout "\n\nWithout unicode: ----------------------\n"
  printDiagnostic stdout False True 4 gccStyle gccLayout diag

-- hPutStrLn stdout "\n\nAs JSON: ------------------------------\n"
-- BS.hPutStr stdout (diagnosticToJson diag)
-- hPutStrLn stdout "\n"

errorNoMarkersNoHints :: Report String
errorNoMarkersNoHints =
  Report
    Error
    Nothing
    "Error with no marker"
    []
    []

errorSingleMarkerNoHints :: Report String
errorSingleMarkerNoHints =
  Report
    Error
    Nothing
    "Error with one marker in bounds"
    [Primary (Range (1, 25) (1, 30) "test.zc") $ Just "Required here"]
    []

warningSingleMarkerNoHints :: Report String
warningSingleMarkerNoHints =
  Report
    Warning
    Nothing
    "Warning with one marker in bounds"
    [Primary (Range (1, 25) (1, 30) "test.zc") $ Just "Required here"]
    []

criticalSingleMarkerNoHints :: Report String
criticalSingleMarkerNoHints =
  Report
    Critical
    Nothing
    "Critical with one marker in bounds"
    [Primary (Range (1, 25) (1, 30) "test.zc") $ Just "Required here"]
    []

errorTwoMarkersSameLineNoOverlapNoHints :: Report String
errorTwoMarkersSameLineNoOverlapNoHints =
  Report
    Error
    Nothing
    "Error with two markers in bounds (no overlap) on the same line"
    [ Primary (Range (1, 5) (1, 10) "test.zc") $ Just "First",
      Secondary (Range (1, 15) (1, 22) "test.zc") $ Just "Second"
    ]
    []

errorSingleMarkerOutOfBoundsNoHints :: Report String
errorSingleMarkerOutOfBoundsNoHints =
  Report
    Error
    Nothing
    "Error with one marker out of bounds"
    [Primary (Range (10, 5) (10, 15) "test2.zc") $ Just "Out of bounds"]
    []

errorTwoMarkersSameLineOverlapNoHints :: Report String
errorTwoMarkersSameLineOverlapNoHints =
  Report
    Error
    Nothing
    "Error with two overlapping markers in bounds"
    [ Primary (Range (1, 6) (1, 13) "test.zc") $ Just "First",
      Secondary (Range (1, 10) (1, 15) "test.zc") $ Just "Second"
    ]
    []

errorTwoMarkersSameLinePartialOverlapNoHints :: Report String
errorTwoMarkersSameLinePartialOverlapNoHints =
  Report
    Error
    Nothing
    "Error with two partially overlapping markers in bounds"
    [ Primary (Range (1, 5) (1, 25) "test.zc") $ Just "First",
      Secondary (Range (1, 12) (1, 20) "test.zc") $ Just "Second"
    ]
    []

errorTwoMarkersTwoLinesNoHints :: Report String
errorTwoMarkersTwoLinesNoHints =
  Report
    Error
    Nothing
    "Error with two markers on two lines in bounds"
    [ Primary (Range (1, 5) (1, 12) "test.zc") $ Just "First",
      Secondary (Range (2, 3) (2, 4) "test.zc") $ Just "Second"
    ]
    []

realWorldExample :: Report String
realWorldExample =
  Report
    Error
    Nothing
    "Could not deduce constraint 'Num(a)' from the current context"
    [ Primary (Range (1, 25) (1, 30) "test.zc") $ Just "While applying function '+'",
      Secondary (Range (1, 11) (1, 16) "test.zc") $ Just "'x' is supposed to have type 'a'",
      Secondary (Range (1, 8) (1, 9) "test.zc") $ Just "type 'a' is bound here without constraints"
    ]
    [Note "Adding 'Num(a)' to the list of constraints may solve this problem." Nothing]

errorTwoMarkersSameRangeNoHints :: Report String
errorTwoMarkersSameRangeNoHints =
  Report
    Error
    Nothing
    "Error with two markers on the same exact position in bounds"
    [ Primary (Range (1, 6) (1, 10) "test.zc") $ Just "First",
      Secondary (Range (1, 6) (1, 10) "test.zc") $ Just "Second"
    ]
    []

errorThreeMarkersWithOverlapNoHints :: Report String
errorThreeMarkersWithOverlapNoHints =
  Report
    Error
    Nothing
    "Error with three markers with overlapping in bounds"
    [ Primary (Range (1, 9) (1, 15) "test.zc") $ Just "First",
      Secondary (Range (1, 9) (1, 18) "test.zc") $ Just "Second",
      Secondary (Range (1, 6) (1, 10) "test.zc") $ Just "Third"
    ]
    []

errorWithMultilineErrorNoMarkerNoHints :: Report String
errorWithMultilineErrorNoMarkerNoHints =
  Report
    Error
    Nothing
    "Error with multi\nline message and no markers"
    []
    []

errorSingleMultilineMarkerMessageNoHints :: Report String
errorSingleMultilineMarkerMessageNoHints =
  Report
    Error
    Nothing
    "Error with single marker with multiline message"
    [Primary (Range (1, 9) (1, 15) "test.zc") $ Just "First\nmultiline"]
    []

errorTwoMarkersSameOriginOverlapNoHints :: Report String
errorTwoMarkersSameOriginOverlapNoHints =
  Report
    Error
    Nothing
    "Error with two markers with same origin but partial overlap in bounds"
    [ Primary (Range (1, 9) (1, 15) "test.zc") $ Just "First",
      Secondary (Range (1, 9) (1, 20) "test.zc") $ Just "Second"
    ]
    []

errorNoMarkersSingleHint :: Report String
errorNoMarkersSingleHint =
  Report
    Error
    Nothing
    "Error with no marker and one hint"
    []
    [Note "First hint" Nothing]

errorNoMarkersSingleMultilineHint :: Report String
errorNoMarkersSingleMultilineHint =
  Report
    Error
    Nothing
    "Error with no marker and one multiline hint"
    []
    [Note "First multi\nline hint" Nothing]

errorNoMarkersTwoHints :: Report String
errorNoMarkersTwoHints =
  Report
    Error
    Nothing
    "Error with no markers and two hints"
    []
    [ Note "First note" Nothing,
      Hint "Second hint" Nothing
    ]

errorSingleMultilineMarkerNoHints :: Report String
errorSingleMultilineMarkerNoHints =
  Report
    Error
    Nothing
    "Error with single marker spanning across multiple lines"
    [Primary (Range (1, 15) (2, 6) "test.zc") $ Just "First"]
    []

errorTwoMarkersWithMultilineNoHints :: Report String
errorTwoMarkersWithMultilineNoHints =
  Report
    Error
    Nothing
    "Error with two markers, one single line and one multiline, in bounds"
    [ Primary (Range (1, 9) (1, 13) "test.zc") $ Just "First",
      Secondary (Range (1, 14) (2, 6) "test.zc") $ Just "Second"
    ]
    []

errorTwoMultilineMarkersNoHints :: Report String
errorTwoMultilineMarkersNoHints =
  Report
    Error
    Nothing
    "Error with two multiline markers in bounds"
    [ Primary (Range (1, 9) (2, 5) "test.zc") $ Just "First",
      Secondary (Range (2, 1) (3, 10) "test.zc") $ Just "Second"
    ]
    []

errorSingleMultilineMarkerMultilineMessageNoHints :: Report String
errorSingleMultilineMarkerMultilineMessageNoHints =
  Report
    Error
    Nothing
    "Error with one multiline marker with a multiline message in bounds"
    [Primary (Range (1, 9) (2, 5) "test.zc") $ Just "Multi\nline message"]
    []

errorTwoMultilineMarkersFirstMultilineMessageNoHints :: Report String
errorTwoMultilineMarkersFirstMultilineMessageNoHints =
  Report
    Error
    Nothing
    "Error with two multiline markers with one multiline message in bounds"
    [ Primary (Range (1, 9) (2, 5) "test.zc") $ Just "First",
      Secondary (Range (1, 9) (2, 6) "test.zc") $ Just "Multi\nline message"
    ]
    []

errorThreeMultilineMarkersTwoMultilineMessageNoHints :: Report String
errorThreeMultilineMarkersTwoMultilineMessageNoHints =
  Report
    Error
    Nothing
    "Error with three multiline markers with two multiline messages in bounds"
    [ Primary (Range (1, 9) (2, 5) "test.zc") $ Just "First",
      Secondary (Range (1, 9) (2, 6) "test.zc") $ Just "Multi\nline message"
    ]
    []

errorOrderSensitive :: Report String
errorOrderSensitive =
  Report
    Error
    Nothing
    "Order-sensitive labels with crossing"
    [ Primary (Range (1, 1) (1, 7) "somefile.zc") $ Just "Leftmost label",
      Secondary (Range (1, 9) (1, 16) "somefile.zc") $ Just "Rightmost label"
    ]
    []

beautifulExample :: Report String
beautifulExample =
  Report
    Error
    Nothing
    "Could not deduce constraint 'Num(a)' from the current context"
    [ Primary (Range (1, 25) (2, 6) "somefile.zc") $ Just "While applying function '+'",
      Secondary (Range (1, 11) (1, 16) "somefile.zc") $ Just "'x' is supposed to have type 'a'",
      Secondary (Range (1, 8) (1, 9) "somefile.zc") $ Just "type 'a' is bound here without constraints"
    ]
    [ Hint
        "Adding 'Num(a)' to the list of constraints may solve this problem."
        (Just (AddCode (1, 9) "somefile.zc" 9 " | Num(a)"))
    ]

errorMultilineAfterSingleLine :: Report String
errorMultilineAfterSingleLine =
  Report
    Error
    Nothing
    "Multiline after single line"
    [ Secondary (Range (1, 17) (1, 18) "unsized.nst") $ Just "Kind is infered from here",
      Primary (Range (2, 14) (3, 0) "unsized.nst") $ Just "is an error"
    ]
    []

errorOnEmptyLine :: Report String
errorOnEmptyLine =
  Report
    Error
    Nothing
    "Error on empty line"
    [Primary (Range (1, 5) (3, 8) "err.nst") $ Just "error on empty line"]
    []

errorMultipleFiles :: Report String
errorMultipleFiles =
  Report
    Error
    Nothing
    "Error on multiple files"
    [ Secondary (Range (1, 5) (1, 7) "test.zc") $ Just "Function already declared here",
      Primary (Range (1, 5) (1, 7) "somefile.zc") $ Just "Function `id` is already declared in another module"
    ]
    []

errorWithCode :: Report String
errorWithCode =
  Report
    Error
    (Just "E0123")
    "Error with code and markers"
    [Primary (Range (1, 5) (1, 7) "test.zc") $ Just "is an error"]
    []

errorWithStrangeUnicodeInput :: Report String
errorWithStrangeUnicodeInput =
  Report
    Error
    (Just "❎")
    "ⓈⓉⓇⒶⓃⒼⒺ ⓊⓃⒾⒸⓄⒹⒺ"
    [ Primary (Range (1, 1) (1, 7) "unicode.txt") $ Just "should work fine 🎉",
      Secondary (Range (1, 7) (1, 9) "unicode.txt") $ Just "After TAB"
    ]
    []

errorWithMultilineMarkerOn3Lines :: Report String
errorWithMultilineMarkerOn3Lines =
  Report
    Error
    Nothing
    "Multiline marker on 3 lines"
    [Primary (Range (1, 3) (3, 10) "test.zc") $ Just "should color all 3 lines correctly"]
    []

errorMultilineMarkerNotAtEnd :: Report String
errorMultilineMarkerNotAtEnd =
  Report
    Error
    Nothing
    "Multiline marker not at end of report"
    [ Primary (Range (1, 10) (2, 3) "test.zc") $ Just "is a multline marker",
      Secondary (Range (3, 5) (3, 13) "test.zc") $ Just "inline marker found after"
    ]
    []

errorWithLineGap :: Report String
errorWithLineGap =
  Report
    Error
    Nothing
    "Error with line gaps between two markers"
    [ Secondary (Range (1, 1) (1, 3) "gaps.txt") $ Just "is a first marker",
      Primary (Range (5, 2) (5, 4) "gaps.txt") $ Just "is the main marker"
    ]
    []

errorWithMultilineMarkerMessage :: Report String
errorWithMultilineMarkerMessage =
  Report
    Error
    Nothing
    "Error with multiline message in first marker"
    [ Primary (Range (1, 5) (1, 10) "test.zc") $ Just "First\nmarker",
      Secondary (Range (1, 15) (1, 22) "test.zc") $ Just "Second"
    ]
    []

errorWithMultilineMarkerMessage' :: Report String
errorWithMultilineMarkerMessage' =
  Report
    Error
    Nothing
    "Error with multiline message in first marker"
    [ Primary (Range (1, 5) (1, 10) "test.zc") $ Just "First\nmarker",
      Secondary (Range (1, 15) (1, 22) "test.zc") $ Just "Second"
    ]
    []

repro3 :: Report String
repro3 =
  Report
    Error
    (Just "WrongStaticLayerLength")
    "The length of the static layer does not\nmatch the length of the template it uses"
    [ Secondary (Range (3, 3) (5, 16) "repro3.file") $ Just "This template has 9 elements",
      Primary (Range (24, 28) (24, 39) "repro3.file") $ Just "... but this layer only has 3 members",
      Secondary (Range (24, 21) (24, 26) "repro3.file") $ Just "This is the template being used",
      Secondary (Range (24, 7) (24, 15) "repro3.file") $ Just "while checking this static layer"
    ]
    []

errorWithSingleBlankMarker :: Report String
errorWithSingleBlankMarker =
  Report
    Error
    Nothing
    "Error with a single blank marker"
    [Blank (Range (1, 5) (1, 10) "test.zc")]
    []

errorWithBlankAndNormalMarkerInLine :: Report String
errorWithBlankAndNormalMarkerInLine =
  Report
    Error
    Nothing
    "Error with a single blank marker"
    [ Blank (Range (1, 5) (1, 10) "test.zc"),
      Primary (Range (1, 15) (1, 22) "test.zc") $ Just "After a blank"
    ]
    []

errorWithAnnotationMarkersInNotes :: Report String
errorWithAnnotationMarkersInNotes =
  Report
    Error
    Nothing
    "Error with a single primary marker and an annotation in a note"
    [Primary (Range (1, 5) (1, 10) "test.zc") $ Just "Because why not"]
    [ Note
        "This contains additional information"
        (Just (Annotate (Range (1, 15) (1, 22) "test.zc") $ Just "hello!"))
    ]

errorWithMultipleMarkersInNotes :: Report String
errorWithMultipleMarkersInNotes =
  Report
    Error
    Nothing
    "Error with a single primary marker and multiple markers in a note"
    [Primary (Range (1, 5) (1, 10) "test.zc") $ Just "Because why not"]
    [ Note
        "This contains additional information"
        (Just (RemoveCode (Range (1, 10) (1, 16) "test.zc")))
    ]

errorWithMultilineCodeAdditionInNote :: Report String
errorWithMultilineCodeAdditionInNote =
  Report
    Error
    Nothing
    "Error with a multiline code addition in a note"
    [Primary (Range (1, 5) (1, 10) "test.zc") $ Just "Because why not"]
    [ Note
        "This contains additional information"
        (Just (AddCode (1, 15) "test.zc" 12 "Hello\nWorld "))
    ]

errorWithAMarkerOnFiveLines :: Report String
errorWithAMarkerOnFiveLines =
  Report
    Error
    Nothing
    "Error with a marker on five lines"
    [Primary (Range (1, 2) (6, 3) "gaps.txt") $ Just "Should take all 6 lines"]
    []

ariadneReadmeExample :: Report String
ariadneReadmeExample =
  Report
    Error
    (Just "E03")
    "Incompatible types"
    [ Primary (Range (1, 12) (4, 2) "sample.tao") $ Just "The values are outputs of this match expression",
      Secondary (Range (2, 11) (2, 12) "sample.tao") $ Just "This is of type Nat",
      Secondary (Range (3, 11) (3, 14) "sample.tao") $ Just "This is of type Str"
    ]
    [Note "Outputs of match expressions must coerce to the same type" Nothing]

criticalFailureExample :: Report String
criticalFailureExample =
  Report
    Critical
    Nothing
    "Encountered an impossible case…"
    [Primary (Range (1, 25) (1, 30) "test.zc") $ Just "While typechecking this expression"]
    [ Note "The metavariable _22 could not be found in the metacontext." $
        Just (Annotate (Range (1, 20) (1, 21) "test.zc") $ Just "It originated from here"),
      Hint
        "The typechecker gave up on your input.\nPlease report this at <website> if it isn't already.\nDon't forget to include this exact error message!"
        Nothing
    ]
