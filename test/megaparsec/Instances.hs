{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Instances where

import Data.Void (Void)
import Error.Diagnose
import Error.Diagnose.Compat.Megaparsec

instance HasHints Void msg where
  hints _ = mempty
