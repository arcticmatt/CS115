--
-- Lab2Tests.hs
--

import qualified PriorityQueue as P
import qualified HeapSort as H
import qualified Data.List as L

import Control.Monad
import Test.HUnit
import Test.QuickCheck

--
-- Priority queue tests.
--

pqTest1 = TestCase $ assertBool "empty queue" (P.isEmpty P.empty)
pqTest2 = TestCase $ assertBool "empty queue 2" 
                       (P.isEmpty (P.fromList ([] :: [Int])))

pqTests = TestList [ TestLabel "PQ test 1" pqTest1
                   , TestLabel "PQ test 2" pqTest2
                   ]

-- The minimum of a singleton priority queue is just the only element.
prop_singleton_1 :: Int -> Bool
prop_singleton_1 i = P.findMin (P.insert i P.empty) == Just i

-- Deleting the minimum of a singleton priority queue is gives an empty
-- priority queue.
prop_singleton_2 :: Int -> Bool
prop_singleton_2 i = 
  case P.deleteMin (P.insert i P.empty) of
    Nothing -> False  -- this shouldn't return Nothing in this case
    Just q -> P.isEmpty q

-- Is the minimum element <= all other elements?
prop_ordered :: [Int] -> Bool
prop_ordered lst =
  let pq = P.fromList lst in iter pq
  where
    iter :: P.Pqueue Int -> Bool
    iter pq = 
      case P.popMin pq of
        Nothing -> True
        Just (i, rest) -> iter1 i rest
    
    iter1 :: Int -> P.Pqueue Int -> Bool
    iter1 i pq = 
      case P.popMin pq of
        Nothing -> True
        Just (i', rest) ->
          if i <= i'
             then iter1 i' rest
             else False

-- A priority queue respects its internal invariants.
prop_valid :: [Int] -> Bool
prop_valid lst = P.isValid $ P.fromList lst

allPQTests :: IO ()
allPQTests = do
  void $ runTestTT pqTests
  quickCheck prop_singleton_1
  quickCheck prop_singleton_2
  quickCheck prop_ordered
  quickCheck prop_valid

--
-- Heap sort tests.
--

sortTest1 = TestCase $ assertEqual "empty list" (H.sort ([] :: [Int])) []
sortTest2 = let lst = [42] in
  TestCase $ assertEqual "list 2" (H.sort lst) (L.sort lst)
sortTest3 = let lst = [5,4,3,2,1,2,3,4,5] in
  TestCase $ assertEqual "list 3" (H.sort lst) (L.sort lst)
sortTest4 = let lst = [5,5,5,5,5,5] in
  TestCase $ assertEqual "list 4" (H.sort lst) (L.sort lst)
sortTests = TestList [ TestLabel "sort test 1" sortTest1
                     , TestLabel "sort test 2" sortTest2
                     , TestLabel "sort test 3" sortTest3
                     , TestLabel "sort test 4" sortTest4
                     ]

prop_sort :: [Int] -> Bool
prop_sort lst = H.sort lst == L.sort lst

allSortTests :: IO ()
allSortTests = do
  void $ runTestTT sortTests
  quickCheckWith (stdArgs { maxSuccess = 1000 }) prop_sort


-- Entry point.
main :: IO ()
main = do
  allPQTests
  allSortTests

