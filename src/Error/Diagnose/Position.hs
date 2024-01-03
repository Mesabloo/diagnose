{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Error.Diagnose.Diagnostic
-- Description : Defines location information as a simple record.
-- Copyright   : (c) Mesabloo, 2021-2022
-- License     : BSD3
-- Stability   : experimental
-- Portability : Portable
module Error.Diagnose.Position (Position (..)) where

#ifdef USE_AESON
import Data.Aeson (ToJSON(..), object, (.=))
#endif
import Data.Default.Class (Default, def)
import Data.Hashable (Hashable)
import GHC.Generics (Generic (..))
import Prettyprinter (Pretty (..), colon)

-- import Text.PrettyPrint.ANSI.Leijen (Pretty(..), text, colon, int)

-- | Contains information about the location of something.
--
--   It is best used in a datatype like:
--
--   > data Located a
--   >   = a :@ Position
--   >   deriving (Show, Eq, Ord, Functor, Traversable)
--
--   Columns are specified in amount of Unicode codepoints from the beginning of the line.
--   Lines and columns start at 1.
data Position = Position
  { -- | The beginning line and column of the span.
    begin :: (Int, Int),
    -- | The end line and column of the span.
    end :: (Int, Int),
    -- | The file this position spans in.
    file :: FilePath
  }
  deriving (Show, Eq, Ord, Generic)

instance Pretty Position where
  pretty (Position (bl, bc) (el, ec) f) = pretty f <> at <> pretty bl <> colon <> pretty bc <> dash <> pretty el <> colon <> pretty ec
    where
      at = pretty @String "@"
      dash = pretty @String "-"

instance Hashable Position

instance Default Position where
  def = Position (1, 1) (1, 1) "<no-file>"

#ifdef USE_AESON
instance ToJSON Position where
  toJSON (Position (bl, bc) (el, ec) file) =
    object [ "beginning" .= object [ "line" .= bl, "column" .= bc ]
           , "end" .= object [ "line" .= el, "column" .= ec ]
           , "file" .= file
           ]
#endif
