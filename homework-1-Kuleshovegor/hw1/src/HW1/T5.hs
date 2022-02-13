module HW1.T5
  ( joinWith
  , splitOn
  ) where

import Data.Foldable (Foldable (foldl'))
import Data.List.NonEmpty (NonEmpty (..), head, tail, (<|))

-- | Splits a list into sublists by a separator.
splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn _ [] = ([] :| [])
splitOn separator (element : xs)
  | element == separator = ([] <| splitOn separator xs)
  | otherwise =
    let curResult = splitOn separator xs
    in ( (element : Data.List.NonEmpty.head curResult) :| Data.List.NonEmpty.tail curResult)

-- | It is the inverse of splitOn function. Join sublists by a separator.
joinWith :: a -> NonEmpty [a] -> [a]
joinWith separator (x :| xs) = foldl' (\element result -> element ++ separator : result) x xs
