{-# LANGUAGE CPP #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

#ifdef USE_AESON
import qualified Data.ByteString.Lazy as BS
import Error.Diagnose(diagnosticToJson)
#endif
import Error.Diagnose
  ( Marker (..),
    Note (..),
    Position (..),
    Report(..),
    addFile,
    addReport,
    defaultStyle,
    printDiagnostic,
    printDiagnostic',
    stdout,
    WithUnicode (..),
    TabSize (..),
  )
import System.IO (hPutStrLn)
import Prettyprinter (Doc, annotate, pretty, hsep, indent, vsep, nest, (<+>), align, list)
import Prettyprinter.Util (reflow)
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..), color, bold, italicized, underlined)
import Data.Traversable (mapAccumL)
import Data.Functor.Compose (Compose(..))
import Data.Foldable (Foldable(..))

main :: IO ()
main = do
  let files :: [(FilePath, String)] =
          [ ("test.zc", "let id<a>(x : a) : a := x + 1\nrec fix(f) := f(fix(f))\nlet const<a, b>(x : a, y : b) : a := x"),
            ("somefile.zc", "let id<a>(x : a) : a := x\n  + 1"),
            ("err.nst", "\n\n\n\n    = jmp g\n\n    g: forall(s: Ts, e: Tc).{ %r0: *s64 | s -> e }"),
            ("unsized.nst", "main: forall(a: Ta, s: Ts, e: Tc).{ %r5: forall().{| s -> e } | s -> %r5 }\n    = salloc a\n    ; sfree\n"),
            ("unicode.txt", "±⅀\t★♲♥🎉汉⑳⓴ჳᏁℳ爪"),
            ("gaps.txt", "abc\ndef\nghi\njkl\nmno\npqr"),
            ("repro3.file", "\n\n  ayo yoa\n    a b\n      c d e f g\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\nlayer qwertyy using yoooo: \"a\" \"b\" \"c\"\n")
          ]

  let reports =
        [ errorNoMarkersNoHints,
          errorSingleMarkerNoHints,
          warningSingleMarkerNoHints,
          errorTwoMarkersSameLineNoOverlapNoHints,
          errorSingleMarkerOutOfBoundsNoHints,
          errorTwoMarkersSameLineOverlapNoHints,
          errorTwoMarkersSameLinePartialOverlapNoHints,
          errorTwoMarkersTwoLinesNoHints,
          realWorldExample,
          errorTwoMarkersSamePositionNoHints,
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
          beautifulExample
        ]
      customAnnReports =
        [ colorfulReport,
          indentedReport,
          nestingReport
        ]

  let diag = foldl' (fmap uncurry addFile) (foldl addReport mempty reports) files
      customDiag = foldl' (fmap uncurry addFile) (foldl addReport mempty customAnnReports) files

  hPutStrLn stdout "\n\nWith unicode: ─────────────────────────\n"
  printDiagnostic stdout WithUnicode (TabSize 4) defaultStyle diag
  hPutStrLn stdout "\n\nWithout unicode: ----------------------\n"
  printDiagnostic stdout WithoutUnicode (TabSize 4) defaultStyle diag
  hPutStrLn stdout "\n\nWith custom annotations: ----------------------\n"
  printDiagnostic' stdout WithUnicode (TabSize 4) defaultStyle customDiag
#ifdef USE_AESON
  hPutStrLn stdout "\n\nAs JSON: ------------------------------\n"
  BS.hPutStr stdout (diagnosticToJson diag)
#endif
  hPutStrLn stdout "\n"

colorfulReport :: Report (Doc AnsiStyle)
colorfulReport =
  fmap hsep
    . getCompose
    . snd
    . mapAccumL
      (\(c : cs) s -> (cs, annotate c (pretty s)))
      styles
    . Compose
    . fmap words
    $ realWorldExample
 where
  styles = [ color fg <> e
      | fg <- cycle [Black, Red, Green, Yellow, Blue, Magenta, Cyan, White]
      | e <- cycle [bold, italicized, underlined]
      ]

indentedReport :: Report (Doc AnsiStyle)
indentedReport =
  Err
    Nothing
    ("Indent..." <> indent 3 (vsep ["foo", "bar", "baz"]))
    [ (Position (1, 15) (1, 16) "test.zc", Maybe a)
    , (Position (1, 11) (1, 12) "test.zc", This b)
    ]
    [Note c]
 where
  a =
    vsep
      [ "A woman’s face with Nature’s own hand painted"
      , "Hast thou, the master-mistress of my passion;"
      , "A woman’s gentle heart, but not acquainted"
      , "With shifting change, as is false women’s fashion;"
      ]
  b =
    vsep
      [ "An eye more bright than theirs, less false in rolling,"
      , "Gilding the object whereupon it gazeth;"
      , "A man in hue, all “hues” in his controlling,"
      , "Which steals men’s eyes and women’s souls amazeth."
      ]
  c =
    vsep
      [ "And for a woman wert thou first created;"
      , "Till Nature, as she wrought thee, fell a-doting,"
      , "And by addition me of thee defeated,"
      , "By adding one thing to my purpose nothing."
      , indent 4 "But since she prick’d thee out for women’s pleasure,"
      , indent 4 "Mine be thy love and thy love’s use their treasure."
      ]

nestingReport :: Report (Doc AnsiStyle)
nestingReport =
  Err
    Nothing
    (nest 4 $ vsep ["Nest...", "foo", "bar", "baz"])
    [ (Position (1, 15) (1, 16) "test.zc", Maybe a)
    ]
    [Note b, Hint c]
 where
  a =
    nest 3 $
      vsep
        [ "'What day is it?' asked Pooh."
        , "'It's today,' squeaked Piglet."
        , "'My favourite day,' said Pooh."
        ]
  b =
    foldr1
      (\p q -> nest 2 (vsep [p, q]))
      [ "It's a very funny thought that, if Bears were Bees,"
      , "They'd build their nests at the bottom of trees."
      , "And that being so (if the Bees were Bears),"
      , "We shouldn't have to climb up all these stairs."
      ]
  c =
    "The elements:"
      <+> align
        ( list
            [ "antimony"
            , "arsenic"
            , "aluminum"
            , "selenium"
            , "hydrogen"
            , "oxygen"
            , "nitrogen"
            , "rhenium"
            , align $ reflow "And there may be many others, but they haven't been discovered"
            ]
        )


errorNoMarkersNoHints :: Report String
errorNoMarkersNoHints =
  Err
    Nothing
    "Error with no marker"
    []
    []

errorSingleMarkerNoHints :: Report String
errorSingleMarkerNoHints =
  Err
    Nothing
    "Error with one marker in bounds"
    [(Position (1, 25) (1, 30) "test.zc", This "Required here")]
    []

warningSingleMarkerNoHints :: Report String
warningSingleMarkerNoHints =
  Warn
    Nothing
    "Warning with one marker in bounds"
    [(Position (1, 25) (1, 30) "test.zc", This "Required here")]
    []

errorTwoMarkersSameLineNoOverlapNoHints :: Report String
errorTwoMarkersSameLineNoOverlapNoHints =
  Err
    Nothing
    "Error with two markers in bounds (no overlap) on the same line"
    [ (Position (1, 5) (1, 10) "test.zc", This "First"),
      (Position (1, 15) (1, 22) "test.zc", Where "Second")
    ]
    []

errorSingleMarkerOutOfBoundsNoHints :: Report String
errorSingleMarkerOutOfBoundsNoHints =
  Err
    Nothing
    "Error with one marker out of bounds"
    [(Position (10, 5) (10, 15) "test2.zc", This "Out of bounds")]
    []

errorTwoMarkersSameLineOverlapNoHints :: Report String
errorTwoMarkersSameLineOverlapNoHints =
  Err
    Nothing
    "Error with two overlapping markers in bounds"
    [ (Position (1, 6) (1, 13) "test.zc", This "First"),
      (Position (1, 10) (1, 15) "test.zc", Where "Second")
    ]
    []

errorTwoMarkersSameLinePartialOverlapNoHints :: Report String
errorTwoMarkersSameLinePartialOverlapNoHints =
  Err
    Nothing
    "Error with two partially overlapping markers in bounds"
    [ (Position (1, 5) (1, 25) "test.zc", This "First"),
      (Position (1, 12) (1, 20) "test.zc", Where "Second")
    ]
    []

errorTwoMarkersTwoLinesNoHints :: Report String
errorTwoMarkersTwoLinesNoHints =
  Err
    Nothing
    "Error with two markers on two lines in bounds"
    [ (Position (1, 5) (1, 12) "test.zc", This "First"),
      (Position (2, 3) (2, 4) "test.zc", Where "Second")
    ]
    []

realWorldExample :: Report String
realWorldExample =
  Err
    Nothing
    "Could not deduce constraint 'Num(a)' from the current context"
    [ (Position (1, 25) (1, 30) "test.zc", This "While applying function '+'"),
      (Position (1, 11) (1, 16) "test.zc", Where "'x' is supposed to have type 'a'"),
      (Position (1, 8) (1, 9) "test.zc", Where "type 'a' is bound here without constraints")
    ]
    ["Adding 'Num(a)' to the list of constraints may solve this problem."]

errorTwoMarkersSamePositionNoHints :: Report String
errorTwoMarkersSamePositionNoHints =
  Err
    Nothing
    "Error with two markers on the same exact position in bounds"
    [ (Position (1, 6) (1, 10) "test.zc", This "First"),
      (Position (1, 6) (1, 10) "test.zc", Maybe "Second")
    ]
    []

errorThreeMarkersWithOverlapNoHints :: Report String
errorThreeMarkersWithOverlapNoHints =
  Err
    Nothing
    "Error with three markers with overlapping in bounds"
    [ (Position (1, 9) (1, 15) "test.zc", This "First"),
      (Position (1, 9) (1, 18) "test.zc", Maybe "Second"),
      (Position (1, 6) (1, 10) "test.zc", Where "Third")
    ]
    []

errorWithMultilineErrorNoMarkerNoHints :: Report String
errorWithMultilineErrorNoMarkerNoHints =
  Err
    Nothing
    "Error with multi\nline message and no markers"
    []
    []

errorSingleMultilineMarkerMessageNoHints :: Report String
errorSingleMultilineMarkerMessageNoHints =
  Err
    Nothing
    "Error with single marker with multiline message"
    [(Position (1, 9) (1, 15) "test.zc", This "First\nmultiline")]
    []

errorTwoMarkersSameOriginOverlapNoHints :: Report String
errorTwoMarkersSameOriginOverlapNoHints =
  Err
    Nothing
    "Error with two markers with same origin but partial overlap in bounds"
    [ (Position (1, 9) (1, 15) "test.zc", This "First"),
      (Position (1, 9) (1, 20) "test.zc", Maybe "Second")
    ]
    []

errorNoMarkersSingleHint :: Report String
errorNoMarkersSingleHint =
  Err
    Nothing
    "Error with no marker and one hint"
    []
    ["First hint"]

errorNoMarkersSingleMultilineHint :: Report String
errorNoMarkersSingleMultilineHint =
  Err
    Nothing
    "Error with no marker and one multiline hint"
    []
    ["First multi\nline hint"]

errorNoMarkersTwoHints :: Report String
errorNoMarkersTwoHints =
  Err
    Nothing
    "Error with no markers and two hints"
    []
    [ "First note",
      Hint "Second hint"
    ]

errorSingleMultilineMarkerNoHints :: Report String
errorSingleMultilineMarkerNoHints =
  Err
    Nothing
    "Error with single marker spanning across multiple lines"
    [(Position (1, 15) (2, 6) "test.zc", This "First")]
    []

errorTwoMarkersWithMultilineNoHints :: Report String
errorTwoMarkersWithMultilineNoHints =
  Err
    Nothing
    "Error with two markers, one single line and one multiline, in bounds"
    [ (Position (1, 9) (1, 13) "test.zc", This "First"),
      (Position (1, 14) (2, 6) "test.zc", Where "Second")
    ]
    []

errorTwoMultilineMarkersNoHints :: Report String
errorTwoMultilineMarkersNoHints =
  Err
    Nothing
    "Error with two multiline markers in bounds"
    [ (Position (1, 9) (2, 5) "test.zc", This "First"),
      (Position (2, 1) (3, 10) "test.zc", Where "Second")
    ]
    []

errorSingleMultilineMarkerMultilineMessageNoHints :: Report String
errorSingleMultilineMarkerMultilineMessageNoHints =
  Err
    Nothing
    "Error with one multiline marker with a multiline message in bounds"
    [(Position (1, 9) (2, 5) "test.zc", This "Multi\nline message")]
    []

errorTwoMultilineMarkersFirstMultilineMessageNoHints :: Report String
errorTwoMultilineMarkersFirstMultilineMessageNoHints =
  Err
    Nothing
    "Error with two multiline markers with one multiline message in bounds"
    [ (Position (1, 9) (2, 5) "test.zc", This "First"),
      (Position (1, 9) (2, 6) "test.zc", Where "Multi\nline message")
    ]
    []

errorThreeMultilineMarkersTwoMultilineMessageNoHints :: Report String
errorThreeMultilineMarkersTwoMultilineMessageNoHints =
  Err
    Nothing
    "Error with three multiline markers with two multiline messages in bounds"
    [ (Position (1, 9) (2, 5) "test.zc", This "First"),
      (Position (1, 9) (2, 6) "test.zc", Where "Multi\nline message"),
      (Position (1, 9) (2, 7) "test.zc", Maybe "Multi\nline message #2")
    ]
    []

errorOrderSensitive :: Report String
errorOrderSensitive =
  Err
    Nothing
    "Order-sensitive labels with crossing"
    [ (Position (1, 1) (1, 7) "somefile.zc", This "Leftmost label"),
      (Position (1, 9) (1, 16) "somefile.zc", Where "Rightmost label")
    ]
    []

beautifulExample :: Report String
beautifulExample =
  Err
    Nothing
    "Could not deduce constraint 'Num(a)' from the current context"
    [ (Position (1, 25) (2, 6) "somefile.zc", This "While applying function '+'"),
      (Position (1, 11) (1, 16) "somefile.zc", Where "'x' is supposed to have type 'a'"),
      (Position (1, 8) (1, 9) "somefile.zc", Where "type 'a' is bound here without constraints")
    ]
    ["Adding 'Num(a)' to the list of constraints may solve this problem."]

errorMultilineAfterSingleLine :: Report String
errorMultilineAfterSingleLine =
  Err
    Nothing
    "Multiline after single line"
    [ (Position (1, 17) (1, 18) "unsized.nst", Where "Kind is infered from here"),
      (Position (2, 14) (3, 0) "unsized.nst", This "is an error")
    ]
    []

errorOnEmptyLine :: Report String
errorOnEmptyLine =
  Err
    Nothing
    "Error on empty line"
    [(Position (1, 5) (3, 8) "err.nst", This "error on empty line")]
    []

errorMultipleFiles :: Report String
errorMultipleFiles =
  Err
    Nothing
    "Error on multiple files"
    [ (Position (1, 5) (1, 7) "test.zc", Where "Function already declared here"),
      (Position (1, 5) (1, 7) "somefile.zc", This "Function `id` is already declared in another module")
    ]
    []

errorWithCode :: Report String
errorWithCode =
  Err
    (Just "E0123")
    "Error with code and markers"
    [(Position (1, 5) (1, 7) "test.zc", This "is an error")]
    []

errorWithStrangeUnicodeInput :: Report String
errorWithStrangeUnicodeInput =
  Err
    (Just "❎")
    "ⓈⓉⓇⒶⓃⒼⒺ ⓊⓃⒾⒸⓄⒹⒺ"
    [ (Position (1, 1) (1, 7) "unicode.txt", This "should work fine 🎉"),
      (Position (1, 7) (1, 9) "unicode.txt", Where "After TAB")
    ]
    []

errorWithMultilineMarkerOn3Lines :: Report String
errorWithMultilineMarkerOn3Lines =
  Err
    Nothing
    "Multiline marker on 3 lines"
    [(Position (1, 3) (3, 10) "test.zc", This "should color all 3 lines correctly")]
    []

errorMultilineMarkerNotAtEnd :: Report String
errorMultilineMarkerNotAtEnd =
  Err
    Nothing
    "Multiline marker not at end of report"
    [ (Position (1, 10) (2, 3) "test.zc", This "is a multline marker"),
      (Position (3, 5) (3, 13) "test.zc", Where "inline marker found after")
    ]
    []

errorWithLineGap :: Report String
errorWithLineGap =
  Err
    Nothing
    "Error with line gaps between two markers"
    [ (Position (1, 1) (1, 3) "gaps.txt", Where "is a first marker"),
      (Position (5, 2) (5, 4) "gaps.txt", This "is the main marker")
    ]
    []

errorWithMultilineMarkerMessage :: Report String
errorWithMultilineMarkerMessage =
  Err
    Nothing
    "Error with multiline message in first marker"
    [ (Position (1, 5) (1, 10) "test.zc", This "First\nmarker"),
      (Position (1, 15) (1, 22) "test.zc", Where "Second")
    ]
    []

errorWithMultilineMarkerMessage' :: Report String
errorWithMultilineMarkerMessage' =
  Err
    Nothing
    "Error with multiline message in first marker"
    [ (Position (1, 5) (1, 10) "test.zc", This "First\nmarker"),
      (Position (1, 15) (1, 22) "test.zc", Where "Second"),
      (Position (1, 10) (1, 15) "test.zc", Maybe "Third")
    ]
    []

repro3 :: Report String
repro3 =
  Err
    (Just "WrongStaticLayerLength")
    "The length of the static layer does not match the length of the template it uses"
    [ (Position (3, 3) (5, 16) "repro3.file", Where "This template has 9 elements"),
      (Position (24, 28) (24, 39) "repro3.file", This "... but this layer only has 3 members"),
      (Position (24, 21) (24, 26) "repro3.file", Where "This is the template being used"),
      (Position (24, 7) (24, 15) "repro3.file", Where "while checking this static layer")
    ]
    []

errorWithSingleBlankMarker :: Report String
errorWithSingleBlankMarker =
  Err
    Nothing
    "Error with a single blank marker"
    [(Position (1, 5) (1, 10) "test.zc", Blank)]
    []

errorWithBlankAndNormalMarkerInLine :: Report String
errorWithBlankAndNormalMarkerInLine =
  Err
    Nothing
    "Error with a single blank marker"
    [(Position (1, 5) (1, 10) "test.zc", Blank), (Position (1, 15) (1, 22) "test.zc", This "After a blank")]
    []
