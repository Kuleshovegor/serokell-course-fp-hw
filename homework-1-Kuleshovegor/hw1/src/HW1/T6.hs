module HW1.T6
  ( epart
  , mcat
  ) where
import Data.Maybe (fromMaybe)

-- | Map each element of the structure into a monoid (Nothing to mempty),
-- and combine the results with (<>).
mcat :: Monoid a => [Maybe a] -> a
mcat = foldMap (fromMaybe mempty)

-- | Map each element of the structure into a pair of monoids,
-- for left it is (value, mempty),
-- for right it is (mempty, value)
-- and combine the results with (<>).
epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldMap fromEitherToMonoid

-- | Map Either to pair of monoids.
-- For left it is (value, mempty).
-- For right it is (mempty, value).
fromEitherToMonoid :: (Monoid a, Monoid b) => Either a b -> (a, b)
fromEitherToMonoid (Left value)  = (value, mempty)
fromEitherToMonoid (Right value) = (mempty, value)
