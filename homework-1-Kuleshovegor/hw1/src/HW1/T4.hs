module HW1.T4
  ( tfoldr
  , treeToList
  ) where

import HW1.T3 (Tree (..))

-- | Single pass through the tree using the summary function.
tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ curResult Leaf = curResult
tfoldr f curResult (Branch _ leftTree value rightTree) =
  let rightResult = tfoldr f curResult rightTree
      midResult = f value rightResult
   in tfoldr f midResult leftTree

-- | Returns a list of tree values ​​in sorted order.
treeToList :: Tree a -> [a]
treeToList = tfoldr (:) []
