module Data.List.Safe where

import Data.Bifunctor (first)


-- | Analogous to 'Data.List.last', but returns 'Nothing' on an empty list, instead of throwing an error.
safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast l  = Just $ last l

-- | Analogous to `Data.List.head`, but returns 'Nothing' in case of an empty list.
safeHead :: [a] -> Maybe a
safeHead []      = Nothing
safeHead (x : _) = Just x

-- | Analogous tu 'Data.List.!!', but does not throw an error on missing index.
safeIndex :: Int -> [a] -> Maybe a
safeIndex _ []       = Nothing
safeIndex 0 (x : _)  = Just x
safeIndex n (_ : xs)
  | n < 0            = Nothing
  | otherwise        = safeIndex (n - 1) xs

-- | Safely deconstructs a list from the end.
--
--   More efficient than @(init x, last x)@
safeUnsnoc :: [a] -> Maybe ([a], a)
safeUnsnoc []       = Nothing
safeUnsnoc [x]      = Just ([], x)
safeUnsnoc (x : xs) = first (x :) <$> safeUnsnoc xs

-- | Safely deconstructs a list from the beginning, returning 'Nothing' if the list is empty.
safeUncons :: [a] -> Maybe (a, [a])
safeUncons []       = Nothing
safeUncons (x : xs) = Just (x, xs)
