module HW1.T3
  ( Tree (..)
  , mkBranch
  , tFromList
  , tdepth
  , tinsert
  , tmember
  , tsize
  ) where

-- | Self-balancing binary search tree.
data Tree a
  = Leaf        -- Tree leaf
  | Branch      -- Node of the tree
    (Int, Int)  -- (size, depth of tree)
    (Tree a)    -- Left subtree
    a           -- Value
    (Tree a)    -- Right subtree
  deriving Show

-- | Size of the tree, O(1).
tsize :: Tree a -> Int
tsize Leaf                         = 0
tsize (Branch (treeSize, _) _ _ _) = treeSize

-- | Depth of the tree.
tdepth :: Tree a -> Int
tdepth Leaf                          = 0
tdepth (Branch (_, treeDepth) _ _ _) = treeDepth

-- | Check if the element is in the tree, O(log n)
tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember requestValue (Branch _ leftTree value rightTree)
  | requestValue == value = True
  | requestValue < value = tmember requestValue leftTree
  | otherwise = tmember requestValue rightTree

-- | Creates a new tree from left subtree, value and right subtree.
mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch leftTree value rightTree
  = Branch (tsize leftTree + 1 + tsize rightTree, 1 + max (tdepth leftTree)
  (tdepth rightTree))
  leftTree value rightTree

-- | Return difference between
-- the heights of the left and right subtrees.
balance :: Tree a -> Int
balance Leaf                            = 0
balance (Branch _ leftTree _ rightTree) = tdepth leftTree - tdepth rightTree

-- | Returns the tree with adjusted balances.
-- For each vertex, the condition is satisfied
-- that the absolute difference between
-- the heights of the left and right subtrees is less than 2.
correctBalance :: Tree a -> Tree a
correctBalance Leaf = Leaf
correctBalance tree@(Branch _ leftTree _ rightTree)
  | treeBalance == -2 && (rightTreeBalance == 0 || rightTreeBalance == -1)
  = smallLeftRotate tree
  | treeBalance == -2 && rightTreeBalance == 1
  = bigLeftRotate tree
  | treeBalance == 2 && (leftTreeBalance == 0 || leftTreeBalance == 1)
  = smallRightRotate tree
  | treeBalance == 2 && leftTreeBalance == -1
  = bigRightRotate tree
  | otherwise = tree
    where treeBalance = balance tree
          rightTreeBalance = balance rightTree
          leftTreeBalance = balance leftTree

-- | Small left turn avl tree.
smallLeftRotate :: Tree a -> Tree a
smallLeftRotate (Branch _ pTree val1 (Branch _ qTree val2 rTree))
  = mkBranch (mkBranch pTree val1 qTree) val2 rTree
smallLeftRotate _ = error "tree don't have right branch"

-- | Small right turn avl tree.
smallRightRotate :: Tree a -> Tree a
smallRightRotate (Branch _ (Branch _ pTree val2 qTree) val1 rTree)
  = mkBranch pTree val2 (mkBranch qTree val1 rTree)
smallRightRotate _ = error "tree don't have left branch"

-- | Big left turn avl tree.
bigLeftRotate :: Tree a -> Tree a
bigLeftRotate (Branch _ leftTree value rightTree) = smallLeftRotate $
 mkBranch leftTree value (smallRightRotate rightTree)
bigLeftRotate _ = error "tree has no suitable structure"

-- | Big right turn avl tree.
bigRightRotate :: Tree a -> Tree a
bigRightRotate (Branch _ leftTree value rightTree) = smallRightRotate $
 mkBranch (smallLeftRotate leftTree) value rightTree
bigRightRotate _ = error "tree has no suitable structure"

-- | Insert an element into the tree, O(log n)
tinsert :: Ord a => a -> Tree a -> Tree a
tinsert newValue Leaf = mkBranch Leaf newValue Leaf
tinsert newValue tree@(Branch _ leftTree value rightTree)
  | newValue == value = tree
  | newValue < value = correctBalance
  $ mkBranch (tinsert newValue leftTree) value rightTree
  | otherwise = correctBalance
  $ mkBranch leftTree value (tinsert newValue rightTree)

-- | Build a tree from a list, O(n log n)
tFromList :: Ord a => [a] -> Tree a
tFromList = foldr tinsert Leaf
