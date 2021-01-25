module Text.Diagnose.Internal.ReportSize where

-- | The maximum width the diagnostic can span across.
--
--   __NOTE:__ The diagnostic may be printed larger if any line is bigger than
--             this specified width.
maxWidth :: Int
maxWidth = 80
