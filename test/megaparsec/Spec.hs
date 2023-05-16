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
import Error.Diagnose.Compat.Megaparsec
import Instances ()
import qualified Repro6
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char.Lexer as MP

main :: IO ()
main = do
  let filename :: FilePath = "<interactive>"
      content1 :: Text = "0000000123456"
      content2 :: Text = "00000a2223266"

  let res1 = first (errorDiagnosticFromBundle Nothing "Parse error on input" Nothing) $ MP.runParser @Void (MP.some MP.decimal <* MP.eof) filename content1
      res2 = first (errorDiagnosticFromBundle Nothing "Parse error on input" Nothing) $ MP.runParser @Void (MP.some MP.decimal <* MP.eof) filename content2

  case res1 of
    Left diag -> printDiagnostic stdout WithUnicode (TabSize 4) defaultStyle (addFile diag filename (Text.unpack content1) :: Diagnostic String)
    Right res -> print @[Integer] res
  case res2 of
    Left diag -> printDiagnostic stdout WithUnicode (TabSize 4) defaultStyle (addFile diag filename (Text.unpack content2) :: Diagnostic String)
    Right res -> print @[Integer] res

  putStrLn "---------------------------------------------------"

  Repro6.main
