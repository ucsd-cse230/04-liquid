import Test.Tasty
import Common
import CSE230.BSTSort

main :: IO ()
main = runTests
  [ probBST
  ]

probBST :: Score -> TestTree
probBST sc = testGroup "BST"
  [ scoreProp sc ("prop_ord"  , prop_bstSort_elems, 20)
  , scoreProp sc ("prop_elems", prop_bstSort_ord, 20)
  ]

prop_bstSort_elems :: [Int] -> Bool
prop_bstSort_elems xs = olistElems (bstSort xs) == listElems xs

prop_bstSort_ord :: [Int] -> Bool
prop_bstSort_ord xs = isSorted (bstSort xs)

isSorted :: (Ord a) => OList a -> Bool
isSorted (OCons x1 xs@(OCons x2 _)) = x1 <= x2 && isSorted xs
isSorted _                          = True