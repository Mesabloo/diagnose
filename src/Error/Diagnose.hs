module Error.Diagnose
  ( -- $header

    -- * How to use this module
    -- $usage

    -- ** Generating a report
    -- $generate_report

    -- ** Creating diagnostics from reports
    -- $create_diagnostic

    -- *** Pretty-printing a diagnostic
    -- $diagnostic_pretty

    -- *** Exporting a diagnostic to JSON
    -- $diagnostic_json

    -- * Re-exports
    module Export ) where

import Error.Diagnose.Position as Export
import Error.Diagnose.Report as Export
import Error.Diagnose.Diagnostic as Export

{- $header

   This module exports all the needed data types to use this library.
   It should be sufficient to only @import "Error.Diagnose"@.
-}

{- $usage

   This library is intended to provide a very simple way of creating beautiful errors, by exposing
   a small yet simple API to the user.

   The basic idea is that a diagnostic is a collection of reports (which embody errors or warnings) along
   with the files which can be referenced in those reports.
-}

{- $generate_report

   A report contains:

   - A message, to be shown at the top

   - A list of located markers, used to underline parts of the source code and to emphasize it with a message

   - A list of hints, shown at the very bottom

   __Note__: The message type contained in a report is abstracted by a type variable.
             In order to render the report, the message must also be able to be rendered in some way
             (that we'll see later).

   This library allows defining two kinds of reports:

   - Errors, using 'err'

   - Warnings, using 'warn'

   Both take a message, a list of located markers and a list of hints.

   A very simple example is:

   > exampleReport :: Report String
   > exampleReport =
   >   err
   >     -- vv  ERROR MESSAGE
   >     "This is my first error report"
   >     -- vv  MARKERS
   >     [ (Position (1, 3) (1, 8) "some_test.txt", This "Some text under the marker") ]
   >     -- vv  HINTS
   >     []

   In general, 'Position's are returned by either a lexer or a parser, so that you never have to construct them
   directly in the code.

   __Note__: If using any parser library, you will have to convert from the internal positioning system to a 'Position'
             to be able to use this library.

   Markers put in the report can be one of (the colors specified are used only when pretty-printing):

   - A 'This' marker, which is the primary marker of the report.
     While it is allowed to have multiple of these inside one report, it is encouraged not to, because the position at the top of
     the report will only be the one of the /first/ 'This' marker, and because the resulting report may be harder to understand.

         This marker is output in red in an error report, and yellow in a warning report.

   - A 'Where' marker contains additional information/provides context to the error/warning report.
     For example, it may underline where a given variable @x@ is bound to emphasize it.

         This marker is output in blue.

   - A 'Error.Diagnose.Report.Maybe' marker may contain possible fixes (if the text is short, else hints are recommended for this use).

         This marker is output in magenta.
-}

{- $create_diagnostic

   To create a new diagnostic, you need to use its 'Data.Default.Default' instance (which exposes a 'def' function, returning a new empty 'Diagnostic').
   Once the 'Diagnostic' is created, you can use either 'addReport' (which takes a 'Diagnostic' and a 'Report', abstract by the same message type,
   and returns a 'Diagnostic') to insert a new report inside the diagnostic, or 'addFile' (which takes a 'Diagnostic', a 'FilePath' and a @['String']@,
   and returns a 'Diagnostic') to insert a new file reference in the diagnostic.

   You can then either pretty-print the diagnostic obtained (which requires all messages to be instances of the 'Text.PrettyPrint.ANSI.Leijen.Pretty')
   or export it to a lazy JSON 'Data.Bytestring.Lazy.ByteString' (e.g. in a LSP context).
-}

{- $diagnostic_pretty

   'Diagnostic's can be output to any 'System.IO.Handle' using the 'printDiagnostic' function.
   This function takes several parameters:

   - The 'System.IO.Handle' onto which to output the 'Diagnostic'.
     It __must__ be a 'System.IO.Handle' capable of outputting data.

   - A 'Bool' used to indicate whether you want to output the 'Diagnostic' with unicode characters, or simple ASCII characters.

         Here are two examples of the same diagnostic, the first output with unicode characters, and the second output with ASCII characters:

         > [error]: Error with one marker in bounds
         >      ╭─🢒 test.zc@1:25-1:30
         >      │
         >    1 │ let id<a>(x : a) : a := x + 1
         >      •                         ┬────
         >      •                         ╰╸ Required here
         > ─────╯

         > [error]: Error with one marker in bounds
         >      +-> test.zc@1:25-1:30
         >      |
         >    1 | let id<a>(x : a) : a := x + 1
         >      :                         ^----
         >      :                         `- Required here
         > -----+

   - A 'Bool' set to 'False' if you don't want colors in the end result.

   - And finally the 'Diagnostic' to output.
-}

{- $diagnostic_json

   'Diagnostic's can be exported to a JSON record of the following type, using the 'diagnosticToJson' function:

   > { files:
   >     { name: string
   >     , content: string[]
   >     }[]
   > , reports:
   >     { kind: 'error' | 'warning'
   >     , message: string
   >     , markers:
   >         { kind: 'this' | 'where' | 'maybe'
   >         , position:
   >             { beginning: { line: int, column: int }
   >             , end: { line: int, column: int }
   >             , file: string
   >             }
   >         , message: string
   >         }[]
   >     , hints: string[]
   >     }[]
   > }

   This is particularly useful in the context of a LSP server, where outputting or parsing a raw error yields strange results or is unnecessarily complicated.
-}
