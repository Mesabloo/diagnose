{-# LANGUAGE MultiParamTypeClasses #-}

module Error.Diagnose.Compat.Hints where

import Error.Diagnose (Note)
    
-- | A class mapping custom errors of type @e@ with messages of type @msg@.
class HasHints e msg where
  -- | Defines all the hints associated with a given custom error.
  hints :: e -> [Note msg]

-- this is a sane default for 'Void'
-- but this can be redefined
--
-- instance HasHints Void msg where
--   hints _ = mempty
