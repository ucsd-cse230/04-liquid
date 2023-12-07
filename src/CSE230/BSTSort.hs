{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use foldr" #-}

module CSE230.BSTSort where

import qualified Data.Set as S

-- the elements of a list

{-@ inline addElem @-}
addElem :: (Ord a) => a -> S.Set a -> S.Set a
addElem x s = S.union (S.singleton x) s

{-@ measure listElems @-}
listElems :: (Ord a) => [a] -> S.Set a
listElems []     = S.empty
listElems (x:xs) = addElem x (listElems xs)

-------------------------------------------------------------------------------
-- | A Type for Ordered Lists
-------------------------------------------------------------------------------

{-@ data OList a
      = ONil
      | OCons { oHd :: a, oTl :: OList {v: a | oHd <= v } }
  @-}
data OList a
   = ONil
   | OCons a (OList a)
   deriving (Eq, Ord, Show)

okList :: OList Int
okList = OCons 1 (OCons 2 (OCons 3 ONil))

{-@ measure olistElems @-}
olistElems :: (Ord a) => OList a -> S.Set a
olistElems ONil         = S.empty
olistElems (OCons x xs) = addElem x (olistElems xs)

-------------------------------------------------------------------------------
-- | A Type for Binary Search Trees
-------------------------------------------------------------------------------
{-@ data BST a
      = Leaf
      | Node { val   :: a
             , left  :: BST {v:a | v < val}
             , right :: BST {v:a | val <= v}
             }
  @-}
data BST a
  = Leaf
  | Node a (BST a) (BST a)
  deriving (Show)

{-@ measure treeElems @-}
treeElems :: (Ord a) => BST a -> S.Set a
treeElems Leaf         = S.empty
treeElems (Node v l r) = addElem v (S.union (treeElems l) (treeElems r))


okBST :: BST Int
okBST = Node 4
            (Node 2
                (Node 1 Leaf Leaf)
                (Node 3 Leaf Leaf)
            )
            (Node 6
                (Node 5 Leaf Leaf)
                (Node 7 Leaf Leaf)
            )

-------------------------------------------------------------------------------
-- | Converting BST to Ordered Lists
-------------------------------------------------------------------------------
-- HINT: implement the code and write the correct refinement type for `toList`
-- you may need a helper method to suitably `splice` sub-lists together...
toList :: BST a -> OList a
toList Leaf         = ONil
toList (Node v l r) = error "TODO"

-------------------------------------------------------------------------------
-- | Converting lists to BST
-------------------------------------------------------------------------------
-- HINT: implement the code and write the correct refinement type for `fromList`;
-- you may need a helper `insert` function...
fromList :: (Ord a) => [a] -> BST a
fromList []     = Leaf
fromList (x:xs) = error "TODO"

-------------------------------------------------------------------------------
-- | Goal: Write a function `bstSort xs` that sorts a list `xs` by using a BST;
--   the output list should be
--   1. be sorted (i.e. an `OList a`)
--   2. have the same elements as the input list `xs`
-------------------------------------------------------------------------------

{-@ bstSort :: xs:[a] -> {v: OList a | olistElems v == listElems xs} @-}
bstSort :: (Ord a) => [a] -> OList a
bstSort xs = toList (fromList xs)