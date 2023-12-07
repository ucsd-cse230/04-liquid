import Test.Tasty
import Common
import CSE230.BSTSort
import qualified CSE230.Pointers as P

main :: IO ()
main = runTests
  [ -- probBST ,
    probPointers
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

probPointers :: Score -> TestTree
probPointers sc = testGroup "ByteString"
  [
    scoreTest (\_ -> P.bLen P.bsTaco       ,  (),     4,  5, "create")
  , scoreTest (\_ -> P.bLen (P.pack "cat") ,  (),     3, 10, "pack")
  , scoreTest (\_ -> P.bLen P.bsRocket     ,  (),     6,  5, "unsafeTake")
  , scoreTest (\_ -> P.bLen P.bsShip       ,  (),     4,  5, "unsafeDrop")
  , scoreTest (\_ -> P.unpack (P.pack "cat"), (), "cat", 20, "unpack")
  ]
    where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)