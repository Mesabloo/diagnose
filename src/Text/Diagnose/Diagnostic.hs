module Text.Diagnose.Diagnostic
( Diagnostic
, diagnostic, (<~<), (<++>)
, printDiagnostic
) where

import Text.Diagnose.Report
import Text.Diagnose.Format
import Data.Map (Map)
import qualified Data.Map as Map
import Text.PrettyPrint.ANSI.Leijen
import System.IO (Handle)

-- | A @'Diagnostic' s m a@ is a diagnostic whose stream is a @s a@ and whose message type is @m@.
data Diagnostic s m a
  = Diagnostic (Files s a) [Report m]


-- | Creates an empty 'Diagnostic' with no files and no reports.
diagnostic :: Diagnostic s m a
diagnostic = Diagnostic mempty mempty

-- | Appends a file along with its name to the map of files of the 'Diagnostic'.
(<~<) :: Diagnostic s m a -> (FilePath, [s a]) -> Diagnostic s m a
Diagnostic files reports <~< (path, content) = Diagnostic (Map.insert path content files) reports

-- | Appends a report to the list of reports of the 'Diagnostic'.
(<++>) :: Diagnostic s m a -> Report m -> Diagnostic s m a
Diagnostic files reports <++> report = Diagnostic files (reports ++ [report])

infixl 5 <++>
infixl 4 <~<


instance (Foldable s, PrettyText (s a), PrettyText m) => PrettyText (Diagnostic s m a) where
  prettyText (Diagnostic files reports) = indent 1 (sep (fmap (prettyReport files) reports)) <> line


-- | Prints a @'Diagnostic' s m a@ To the given @'Handle'@
printDiagnostic :: (Foldable s, PrettyText (s a), PrettyText m) => Handle -> Diagnostic s m a -> IO ()
printDiagnostic handle diag = displayIO handle (renderPretty 0.9 80 $ prettyText diag)
