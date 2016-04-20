module HeapSort where

import qualified PriorityQueue as Q
import Data.Maybe

-- Heap sort implemenation. Pops the minimum element from the heap and appends
-- it to the beginning of the sorted list. The tail of the list is found 
-- recursively. In this way, we build up a list that is sorted in ascending 
-- order.
sort :: Ord a => [a] -> [a]
sort [] = []
sort xs = recur (Q.fromList xs)
    where
        recur :: Ord a => Q.Pqueue a -> [a]
        recur p | Q.isEmpty p = []
                | otherwise = let (min, heap) = fromJust $ Q.popMin p
                              in min : recur heap
