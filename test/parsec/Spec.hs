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
import Error.Diagnose.Layout.GCC (gccLayout)
import Error.Diagnose.Layout.GCC.Style (gccStyle)
-------
import qualified Repro2 as Issue2
import qualified Text.Parsec as P

instance HasHints Void Text where hints _ = mempty

main :: IO ()
main = do
  let filename :: FilePath = "<interactive>"
      content1 :: Text = "0000000123456"
      content2 :: Text = "00000a2223266"
      content3 :: Text = "aab"

  let res1 = first (errorDiagnosticFromParseError Nothing "Parse error on input" Nothing) $ P.parse (P.many1 P.digit <* P.eof) filename content1
      res2 = first (errorDiagnosticFromParseError Nothing "Parse error on input" Nothing) $ P.parse (P.many1 P.digit <* P.eof) filename content2
      res3 = first (errorDiagnosticFromParseError Nothing "Parse error on input" Nothing) $ P.parse (test1 <* P.eof) filename content3

  case res1 of
    Left diag -> printDiagnostic stdout True True 4 gccStyle gccLayout (addFile diag filename (Text.unpack content1) :: Diagnostic String)
    Right res -> print res
  case res2 of
    Left diag -> printDiagnostic stdout True True 4 gccStyle gccLayout (addFile diag filename (Text.unpack content2) :: Diagnostic String)
    Right res -> print res
  case res3 of
    Left diag -> printDiagnostic stdout True True 4 gccStyle gccLayout (addFile diag filename (Text.unpack content3) :: Diagnostic String)
    Right res -> print res

  -- all issue reproduction
  Issue2.main

test1 = P.many (P.string "a") *> P.string "b" *> P.many1 (P.string "c")
