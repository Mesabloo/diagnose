{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      : Error.Diagnose.Diagnostic
-- Description : Defines location information as a simple record.
-- Copyright   : (c) Mesabloo and contributors, 2021-
-- License     : BSD3
-- Stability   : experimental
-- Portability : Portable
module Error.Diagnose.Position (SourceRange (.., Position), Position, SourcePosition) where

#ifdef USE_AESON
import Data.Aeson (ToJSON(..), object, (.=))
#endif
import Data.Default (Default, def)
import Data.Hashable (Hashable)
import Error.Diagnose.Pretty (Pretty (..), colon)
import GHC.Generics (Generic (..))

-- | A source position in Unicode code points.
--
--   The first component records the line, and the second component records the column.
type SourcePosition = (Int, Int)

-- | A deprecated alias to 'SourceRange'.
type Position = SourceRange

{-# DEPRECATED Position "Use 'SourceRange' instead." #-}
pattern Position :: SourcePosition -> SourcePosition -> FilePath -> SourceRange
pattern Position a b c = Range a b c

{-# COMPLETE Position #-}

-- | Contains information about the location of something.
--
--   It is best used in a datatype like:
--
--   > data Located a
--   >   = a :@ SourceRange
--   >   deriving (Show, Eq, Ord, Functor, Traversable)
--
--   Columns are specified in amount of Unicode codepoints from the beginning of the line.
--   Lines and columns start at 1.
data SourceRange = Range
  { -- | The beginning line and column of the span.
    begin :: {-# UNPACK #-} !SourcePosition,
    -- | The end line and column of the span.
    end :: {-# UNPACK #-} !SourcePosition,
    -- | The file this position spans in.
    file :: !FilePath
  }
  deriving (Show, Eq, Generic)

instance Ord Position where
  Position b1 e1 _ `compare` Position b2 e2 _ = (b1, e1) `compare` (b2, e2)

instance Pretty Position where
  pretty (Position (bl, bc) (el, ec) f) = pretty f <> at <> pretty bl <> colon <> pretty bc <> dash <> pretty el <> colon <> pretty ec
    where
      at = pretty @String "@"
      dash = pretty @String "-"

instance Hashable Position

instance Default Position where
  def = Range (1, 1) (1, 1) "<no-file>"

#ifdef USE_AESON
instance {-# OVERLAPPING #-} ToJSON SourcePosition where
  toJSON (l, c) = object [ "line" .= l, "column" .= c ]

instance ToJSON Position where
  toJSON (Range begin end file) =
    object [ "beginning" .= begin
           , "end" .= end
           , "file" .= file
           ]
#endif
