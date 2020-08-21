{-# LANGUAGE RecordWildCards #-}

module Text.Diagnose.Position where

import Text.PrettyPrint.ANSI.Leijen

-- | Offset in a stream used to determine where to put markers.
data Position
  = Position
  { beginning :: (Integer, Integer) -- ^ The beginning line and column
  , end       :: (Integer, Integer) -- ^ The end line and column
  , file      :: String             -- ^ The name of the file (does not need to be an absolute path)
  }
  deriving (Show, Eq)

instance Pretty Position where
  pretty Position{..} =
    let (bLine, bCol) = beginning
    in angles (text file <> colon <> integer bLine <> colon <> integer bCol)

instance Ord Position where
  p1 <= p2 =
    let (b1Line, b1Col) = beginning p1
        (b2Line, b2Col) = beginning p2
    in b1Line <= b2Line && b1Col <= b2Col
