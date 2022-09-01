{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Repro2 where

import Data.Void
import Error.Diagnose
import Error.Diagnose.Compat.Parsec
import Error.Diagnose.Layout.Ariadne (ariadneLayout)
import Text.Parsec
import Text.Parsec.Token

instance HasHints Void String where hints _ = mempty

type Parser = Parsec String ()

diagParse :: Parser a -> SourceName -> String -> Either (Diagnostic String) a
diagParse p filename content =
  either (Left . diag) Right (parse p filename content)
  where
    diag e = addFile (errorDiagnosticFromParseError Nothing "Parse error on input" Nothing e) filename content

parser1 :: Parser Char
parser1 = op "\\" *> letter

parser2 :: Parser Char
parser2 = op' "\\" *> letter

main :: IO ()
main = do
  either (printDiagnostic stderr True True 4 defaultStyle ariadneLayout) print $ diagParse parser1 "issues/2.txt" "\\1"
  either (printDiagnostic stderr True True 4 defaultStyle ariadneLayout) print $ diagParse parser2 "issues/2.txt" "\\1"

-- smaller example
op' :: String -> Parser String
op' name = string name <* spaces

op :: String -> Parser ()
op =
  reservedOp $
    makeTokenParser
      LanguageDef
        { commentStart = "{-",
          commentEnd = "-}",
          commentLine = "--",
          reservedOpNames = ["\\"],
          opStart = oneOf "\\",
          opLetter = oneOf "\\"
        }
