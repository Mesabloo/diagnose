{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS -Wno-orphans #-}

import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import Data.Void (Void)
import Error.Diagnose
import Error.Diagnose.Compat.Parsec
--
import qualified Repro2 as Issue2
--
import qualified Text.Parsec as P

instance HasHints Void Text where hints _ = mempty

main :: IO ()
main = do
  let filename :: FilePath = "<interactive>"
      content1 :: Text = "0000000123456"
      content2 :: Text = "00000a2223266"

  let res1 = first (errorDiagnosticFromParseError Nothing "Parse error on input" Nothing) $ P.parse (P.many1 P.digit <* P.eof) filename content1
      res2 = first (errorDiagnosticFromParseError Nothing "Parse error on input" Nothing) $ P.parse (P.many1 P.digit <* P.eof) filename content2

  case res1 of
    Left diag -> printDiagnostic stdout True True 4 (addFile diag filename (Text.unpack content1) :: Diagnostic String)
    Right res -> print res
  case res2 of
    Left diag -> printDiagnostic stdout True True 4 (addFile diag filename (Text.unpack content2) :: Diagnostic String)
    Right res -> print res

  -- all issue reproduction
  Issue2.main
