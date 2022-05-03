module Error.Diagnose
  ( -- $header

    -- * How to use this module
    -- $usage

    -- ** Generating a report
    -- $generate_report

    -- ** Creating diagnostics from reports
    -- $create_diagnostic

    -- *** Pretty-printing a diagnostic onto a file 'System.IO.Handle'
    -- $diagnostic_pretty

    -- *** Pretty-printing a diagnostic as a 'PrettyPrinter.Doc'ument
    -- $diagnostic_to_doc

    -- *** Exporting a diagnostic to JSON
    -- $diagnostic_json

    -- ** Compatibility layers for popular parsing libraries
    -- $compatibility_layers

    -- *** megaparsec >= 9.0.0 ("Error.Diagnose.Compat.Megaparsec")
    -- $compatibility_megaparsec

    -- *** parsec >= 3.1.14.0 ("Error.Diagnose.Compat.Parsec")
    -- $compatibility_parsec

    -- *** Common errors
    -- $compatibility_errors

    -- * Re-exports
    module Export,
  )
where

import Error.Diagnose.Diagnostic as Export
import Error.Diagnose.Position as Export
import Error.Diagnose.Pretty as Export
import Error.Diagnose.Report as Export
import Error.Diagnose.Style as Export

-- $header
--
--   This module exports all the needed data types to use this library.
--   It should be sufficient to only @import "Error.Diagnose"@.

-- $usage
--
--   This library is intended to provide a very simple way of creating beautiful errors, by exposing
--   a small yet simple API to the user.
--
--   The basic idea is that a diagnostic is a collection of reports (which embody errors or warnings) along
--   with the files which can be referenced in those reports.

-- $generate_report
--
--   A report contains:
--
--   - A message, to be shown at the top
--
--   - A list of located markers, used to underline parts of the source code and to emphasize it with a message
--
--   - A list of hints, shown at the very bottom
--
--   __Note__: The message type contained in a report is abstracted by a type variable.
--             In order to render the report, the message must also be able to be rendered in some way
--             (that we'll see later).
--
--   This library allows defining two kinds of reports:
--
--   - Errors, using 'err'
--
--   - Warnings, using 'warn'
--
--   Both take an optional error code, a message, a list of located markers and a list of hints.
--
--   A very simple example is:
--
--   > exampleReport :: Report String
--   > exampleReport =
--   >   err
--   >     -- vv  OPTIONAL ERROR CODE
--   >     Nothing
--   >     -- vv  ERROR MESSAGE
--   >     "This is my first error report"
--   >     -- vv  MARKERS
--   >     [ (Position (1, 3) (1, 8) "some_test.txt", This "Some text under the marker") ]
--   >     -- vv  HINTS
--   >     []
--
--   In general, 'Position's are returned by either a lexer or a parser, so that you never have to construct them
--   directly in the code.
--
--   __Note__: If using any parser library, you will have to convert from the internal positioning system to a 'Position'
--             to be able to use this library.
--
--   Markers put in the report can be one of (the colors specified are used only when pretty-printing):
--
--   - A 'Error.Diagnose.Report.This' marker, which is the primary marker of the report.
--     While it is allowed to have multiple of these inside one report, it is encouraged not to, because the position at the top of
--     the report will only be the one of the /first/ 'Error.Diagnose.Report.This' marker, and because the resulting report may be harder to understand.
--
--         This marker is output in red in an error report, and yellow in a warning report.
--
--   - A 'Error.Diagnose.Report.Where' marker contains additional information/provides context to the error/warning report.
--     For example, it may underline where a given variable @x@ is bound to emphasize it.
--
--         This marker is output in blue.
--
--   - A 'Error.Diagnose.Report.Maybe' marker may contain possible fixes (if the text is short, else hints are recommended for this use).
--
--         This marker is output in magenta.

-- $create_diagnostic
--
--   To create a new diagnostic, you need to use its 'Data.Default.Default' instance (which exposes a 'def' function, returning a new empty 'Diagnostic').
--   Once the 'Diagnostic' is created, you can use either 'addReport' (which takes a 'Diagnostic' and a 'Report', abstract by the same message type,
--   and returns a 'Diagnostic') to insert a new report inside the diagnostic, or 'addFile' (which takes a 'Diagnostic', a 'FilePath' and a @['String']@,
--   and returns a 'Diagnostic') to insert a new file reference in the diagnostic.
--
--   You can then either pretty-print the diagnostic obtained (which requires all messages to be instances of the 'Prettyprinter.Pretty')
--   -- directly onto a file handle or as a plain 'Prettyprinter.Doc'ument --
--   or export it to a lazy JSON 'Data.Bytestring.Lazy.ByteString' (e.g. in a LSP context).

-- $diagnostic_pretty
--
--   'Diagnostic's can be output to any 'System.IO.Handle' using the 'printDiagnostic' function.
--   This function takes several parameters:
--
--   - The 'System.IO.Handle' onto which to output the 'Diagnostic'.
--     It __must__ be a 'System.IO.Handle' capable of outputting data.
--
--   - A 'Bool' used to indicate whether you want to output the 'Diagnostic' with unicode characters, or simple ASCII characters.
--
--         Here are two examples of the same diagnostic, the first output with unicode characters, and the second output with ASCII characters:
--
--         > [error]: Error with one marker in bounds
--         >      ╭──▶ test.zc@1:25-1:30
--         >      │
--         >    1 │ let id<a>(x : a) : a := x + 1
--         >      •                         ┬────
--         >      •                         ╰╸ Required here
--         > ─────╯
--
--         > [error]: Error with one marker in bounds
--         >      +--> test.zc@1:25-1:30
--         >      |
--         >    1 | let id<a>(x : a) : a := x + 1
--         >      :                         ^----
--         >      :                         `- Required here
--         > -----+
--
--   - A 'Bool' set to 'False' if you don't want colors in the end result.
--
--   - A 'Int' describing the number of spaces with which to output a TAB character.
--
--   - The 'Style' describing colors of the report.
--     See the module "Error.Diagnose.Style" for how to define new styles.
--
--   - And finally the 'Diagnostic' to output.

-- $diagnostic_to_doc
--
-- 'Diagnostic's can be “output” (at least ready to be rendered) to a 'Prettyprinter.Doc', which allows it to be easily added to other 'Prettyprinter.Doc' outputs.
-- This makes it easy to customize the error messages further (though not the internal parts, only adding to it).
-- As a 'Prettyprinter.Doc', there is also the possibility of altering internal annotations (styles) much easier (although this is already possible when printing the diagnostic).

-- $diagnostic_json
--
--   'Diagnostic's can be exported to a JSON record of the following type, using the 'diagnosticToJson' function:
--
--   > { files:
--   >     { name: string
--   >     , content: string[]
--   >     }[]
--   > , reports:
--   >     { kind: 'error' | 'warning'
--   >     , code: string?
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
--
--   This is particularly useful in the context of a LSP server, where outputting or parsing a raw error yields strange results or is unnecessarily complicated.
--
--   Please note that this requires the flag @diagnose:json@ to be enabled (it is disabled by default in order not to include @aeson@, which is a heavy library).

-- $compatibility_layers
--
--   There are many parsing libraries available in the Haskell ecosystem, each coming with its own way of handling errors.
--   Eventually, one needs to be able to map errors from these libraries to 'Diagnostic's, without having to include additional code for doing so.
--   This is where compatibility layers come in handy.
--
--   As of now, there are compatibility layers for these libraries:

-- $compatibility_megaparsec
--
--   This needs the flag @diagnose:megaparsec-compat@ to be enabled.
--
--   Using the compatibility layer is very easy, as it is designed to be as simple as possible.
--   One simply needs to convert the 'Text.Megaparsec.ParseErrorBundle' which is returned by running a parser into a 'Diagnostic' by using 'Error.Diagnose.Compat.Megaparsec.diagnosticFromBundle'.
--   Several wrappers are included for easy creation of kinds (error, warning) of diagnostics.
--
--   __Note:__ the returned diagnostic does not include file contents, which needs to be added manually afterwards.
--
--   As a quick example:
--
--   > import qualified Text.Megaparsec as MP
--   > import qualified Text.Megaparsec.Char as MP
--   > import qualified Text.Megaparsec.Char.Lexer as MP
--   >
--   > let filename = "<interactive>"
--   >     content  = "00000a2223266"
--   >
--   > let myParser = MP.some MP.decimal <* MP.eof
--   >
--   > let res      = MP.runParser myParser filename content
--   >
--   > case res of
--   >   Left bundle ->
--   >     let diag  = errorDiagnosticFromBundle Nothing "Parse error on input" Nothing bundle
--   >            --   Creates a new diagnostic with no default hints from the bundle returned by megaparsec
--   >         diag' = addFile diag filename content
--   >            --   Add the file used when parsing with the same filename given to 'MP.runParser'
--   >     in printDiagnostic stderr True True 4 diag'
--   >   Right res   -> print res
--
--   This example will return the following error message (assuming default instances for @'Error.Diagnose.Compat.Megaparsec.HasHints' 'Data.Void.Void' msg@):
--
--   > [error]: Parse error on input
--   >      ╭──▶ <interactive>@1:6-1:7
--   >      │
--   >    1 │ 00000a2223266
--   >      •      ┬
--   >      •      ├╸ unexpected 'a'
--   >      •      ╰╸ expecting digit, end of input, or integer
--   > ─────╯

-- $compatibility_parsec
--
--   This needs the flag @diagnose:parsec-compat@ to be enabled.
--
--   This compatibility layer allows easily converting 'Text.Parsec.Error.ParseError's into a single-report diagnostic containing all available information such
--   as unexpected/expected tokens or error messages.
--   The function 'Error.Diagnose.Compat.Parsec.diagnosticFromParseError' is used to perform the conversion between a 'Text.Parsec.Error.ParseError' and a 'Diagnostic'.
--
--   __Note:__ the returned diagnostic does not include file contents, which needs to be added manually afterwards.
--
--   Quick example:
--
--   > import qualified Text.Parsec as P
--   >
--   > let filename = "<interactive>"
--   >     content  = "00000a2223266"
--   >
--   > let myParser = P.many1 P.digit <* P.eof
--   >
--   > let res      = P.parse myParser filename content
--   >
--   > case res of
--   >   Left error ->
--   >     let diag  = errorDiagnosticFromParseError Nothing "Parse error on input" Nothing error
--   >            --   Creates a new diagnostic with no default hints from the bundle returned by megaparsec
--   >         diag' = addFile diag filename content
--   >            --   Add the file used when parsing with the same filename given to 'MP.runParser'
--   >     in printDiagnostic stderr True True 4 diag'
--   >   Right res  -> print res
--
--   This will output the following errr on @stderr@:
--
--   > [error]: Parse error on input
--   >      ╭──▶ <interactive>@1:6-1:7
--   >      │
--   >    1 │ 00000a2223266
--   >      •      ┬
--   >      •      ├╸ unexpected 'a'
--   >      •      ╰╸ expecting any of digit, end of input
--   > ─────╯

-- $compatibility_errors
--
--   - @No instance for (HasHints ??? msg) arising from a use of ‘errorDiagnosticFromBundle’@ (@???@ is any type, depending on your parser's custom error type):
--
--       The typeclass 'Error.Diagnose.Compat.Megaparsec.HasHints' does not have any default instances, because treatments of custom errors is highly dependent on who is using the library.
--       As such, you will need to create orphan instances for your parser's error type.
--
--       Note that the message type @msg@ can be left abstract if the implements of 'Error.Diagnose.Compat.Hints.hints' is @hints _ = mempty@.
