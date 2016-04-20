module PriorityQueue(Pqueue,
                     empty,
                     isEmpty,
                     insert,
                     findMin,
                     deleteMin,
                     popMin,
                     fromList,
                     isValid)
where

import Data.Maybe

-- A.1
-- Datatype for priority queues, parameterized around an element type a.
-- The element type should be an instance of the Ord type class.
-- Leaves represent empty heaps and contain no data.
-- Nodes contain: a stored element, a rank (pos. integer), and a left and 
-- right subheap.
data Pqueue a =
    Leaf
  | Node a Int (Pqueue a) (Pqueue a) 

-- A.2
-- An empty priority queue storing values of type a.
empty :: Pqueue a
empty = Leaf

-- A.3
-- Return True if the queue is empty.
isEmpty :: Pqueue a -> Bool
isEmpty Leaf = True
isEmpty (Node _ _ _ _) = False

-- A.4
-- Return the integer rank of the priority queue argument. Does not do 
-- any real computation.
rank :: Pqueue a -> Int 
rank Leaf = 0
rank (Node _ r _ _) = r

-- A.5
-- TODO: think about this. make cleaner?
-- Merges two priority queues (represented as leftist heaps), returning a new 
-- priority queue. The original priority queues are not altered.
-- Here's the algo.
-- If either heap is empty, return the other heap.
-- If the first heap's min elem is smaller than the second heap's min elem,
-- make a new heap from the first heap's min elem, the first heap's left 
-- subheap, and the result of merging the first heap's right subheap with
-- the second heap. 
-- Otherwise, make a new heap from the second heap's min elem, the second heap's
-- left subheap, and the result of merging the first heap with the second heap's
-- right subheap.
merge :: Ord a => Pqueue a -> Pqueue a -> Pqueue a
merge Leaf Leaf = Leaf
merge Leaf p@(Node _ _ _ _) = p
merge p@(Node _ _ _ _) Leaf = p
merge p1@(Node v1 _ left1 right1) p2@(Node v2 _ left2 right2) 
        | v1 < v2 = let merged = (merge right1 p2)
                        leftRank = rank left1
                        merge_rank = rank merged
                        in 
                        if leftRank < merge_rank 
                            then (Node v1 (leftRank + 1) merged left1)
                            else (Node v1 (merge_rank + 1) left1 merged)
        | otherwise = let merged = (merge p1 right2)
                          leftRank = rank left2 
                          merge_rank = rank merged 
                          in 
                          if leftRank < merge_rank 
                              then (Node v2 (leftRank + 1) merged left2)
                              else (Node v2 (merge_rank + 1) left2 merged)
                            
-- A.6
-- Insert an item into a priority queue.
insert :: Ord a => a -> Pqueue a -> Pqueue a
insert v Leaf = merge (Node v 1 Leaf Leaf) Leaf
insert v p@(Node _ _ _ _) = merge (Node v 1 Leaf Leaf) p

-- A.7
-- Find the minimum-valued element in a priority queue if possible.
findMin :: Ord a => Pqueue a -> Maybe a
findMin Leaf = Nothing 
findMin (Node v _ _ _) = Just v

-- A.8
-- Delete the minimum element from a priority queue if possible.
deleteMin :: Ord a => Pqueue a -> Maybe (Pqueue a)
deleteMin Leaf = Nothing 
deleteMin (Node _ _ left right) = Just (merge left right)

-- A.9
-- Remove the minimum element if possible and return it, 
-- along with the rest of the priority queue.
popMin :: Ord a => Pqueue a -> Maybe (a, Pqueue a)
popMin Leaf = Nothing
popMin (Node v _ left right) = Just (v, (merge left right))

-- A.10
-- Convert an unordered list into a priority queue.
fromList :: Ord a => [a] -> Pqueue a
fromList = foldr insert Leaf

-- A.11
-- Validate the internal structure of the priority queue.
-- 5 criteria:
-- 1. A non-leaf node's rank is a positive integer.
-- 2. A non-leaf node's left subheap has a rank which is at least as large 
--    as that of its right subheap.
-- 3. A non-leaf node's rank is one larger than the rank of its right subheap.
-- 4. A non-leaf node's data item is less than or equal to the minimum item 
--    in either subheap.
-- 5. The subheaps are also valid leftist heaps.
-- Note: if left/right heaps are leaves, the data item is automatically "less"
--       than them.
isValid :: Ord a => Pqueue a -> Bool
isValid Leaf = True
isValid (Node v r left right) = 
        let leftRank = rank left 
            rightRank = rank right
            minLeft = findMin left 
            minRight = findMin right 
            lessLeft = not (isJust minLeft) || v <= fromJust minLeft 
            lessRight = not (isJust minRight) || v <= fromJust minRight
        in
        r > 0 && leftRank >= rightRank && r == rightRank + 1
            && lessLeft && lessRight && isValid left && isValid right
