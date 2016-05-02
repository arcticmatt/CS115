-- ==================== Part A ====================

-- A.1.1
(+*) :: Double -> Double -> Double
x +* y = (x ** 2) + (y ** 2)
infixl 7 +*

-- A.1.2
(^||) :: Bool -> Bool -> Bool
False ^|| y = y
True ^|| y = not y
infixr 3 ^||

-- A.2
rangeProduct :: Integer -> Integer -> Integer
rangeProduct a b | a > b = error "invalid range, lower bound greater than \
                                    \upper bound"
                 | a == b = a
                 | otherwise = a * rangeProduct (a + 1) b

-- A.3
prod :: [Integer] -> Integer
prod = foldr (*) 1

rangeProduct2 :: Integer -> Integer -> Integer
rangeProduct2 a b | a > b = error "invalid range, lower bound greater than \
                                    \upper bound"
                  | otherwise = prod [a..b]

-- A.4.1
map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 f (x:xs) (y:ys) = f x y : map2 f xs ys
map2 _ _ _ = []

-- A.4.2
map3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
map3 f (x:xs) (y:ys) (z:zs) = f x y z : map3 f xs ys zs
map3 _ _ _ _ = []

-- A.4.3
{-
  NOTE: map2_t = map2 (*)
  ((sum .) . map2 (*))
  ((sum .) . map2_t) lst1 lst2
  ((\x -> sum . x) . map2_t) lst1 lst2
  ((\x -> sum . x) (map2_t lst1)) lst2
  (sum . (map2_t lst1)) lst2 
  (sum (map2_t lst1 lst2)) 
  sum (map2_t lst1 lst2) <==> sum (map2 (*) lst1 lst2)
-}

-- A.5
nSum = sum [x | x <- [0..999], x `mod` 3 == 0 || x `mod` 5 == 0]
-- nSum = 233168

-- A.6
sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x:xs) = x : sieve (filter (\a -> a `mod` x /= 0) xs)

primes = sieve [2..]

primeSum = sum (takeWhile (< 10000) primes)
-- primeSum = 5736396

-- ==================== Part B ====================

-- B.1
-- The fix is to use pattern matching to deconstruct the list argument.
-- This lets us omit the calls to "head" and "tail", and can be implemented
-- as follows.
sumList :: [Integer] -> Integer
sumList [] = 0
sumList (x:xs) = x + sumList xs

-- B.2
-- The problem is that the length is used in the the pattern guards, causing
-- for unnecessary computations. Also, the usage of "head" and "tail" is
-- not good style; pattern matching should be used to deconstruct the list
-- argument. To fix this, we catch the empty list error in the first pattern
-- matching statements, and then implement a more efficient pattern matching
-- for the remaining cases.
largest :: [Integer] -> Integer
largest [] = error "empty list"
largest [x] = x
largest (x:xs) = max x (largest xs)

-- ==================== Part C ====================

-- C.1
{-
  fib 3
  --> fib (3 - 1) + fib (3 - 2) [expand from definition]
  --> fib 2 + fib (3 - 2) [evaluate leftmost branch of + operator]
  --> (fib (2 - 1) + fib (2 - 2)) + fib (3 - 2)
  --> (fib 1 + fib (2 - 2)) + fib (3 - 2)
  --> (1 + fib (2 - 2)) + fib (3 - 2)
  --> (1 + fib 0) + fib (3 - 2)
  --> (1 + 0) + fib (3 - 2)
  --> 1 + fib (3 - 2)
  --> 1 + fib 1 [evaluate rightmost branch of + operator]
  --> 1 + 1 [evaluate rightmost branch of + operator]
  --> 2 [evaluate rightmost branch of + operator]
-}

-- C.2
{-
  fact 3
  --> 3 * fact (3 - 1) [expand from definition]
  --> 3 * ((3 - 1) * fact ((3 - 1) - 1))
  --> 3 * (2 * fact ((3 - 1) - 1))
  --> 3 * (2 * (((3 - 1) - 1) * fact(((3 - 1) - 1) - 1)))
  --> 3 * (2 * ((2 - 1) * fact(((3 - 1) - 1) - 1)))
  --> 3 * (2 * (1 * fact(((3 - 1) - 1) - 1)))
  --> etc... doesn't terminate
 
  The problem is that the "fact n" line is first. This causes Haskell to 
  pattern match any expression with "n". Thus, a call to fact will never 
  terminate, because the recursive call always gets made and the base case 
  never occurs. To fix this, we can simply move the "fact 0" line above the 
  "fact n" line.
-}

-- C.3
{-
  reverse [1, 2, 3]
  --> iter [1, 2, 3] [] [expand from definition]
  --> iter [2, 3] (1 : [])
  --> iter [3] (2 : (1 : []))
  --> iter [] (3 : (2 : (1 : [])))
  --> (3 : (2 : (1 : [])))
  --> [3, 2, 1]
 
  The asymptotic time complexity of this function is O(N), where N is the
  length of the input list. Each iteration moves (lazily) one element of the 
  input list (the head element) to the reversed list, which is a constant time
  operation (a list construction). Then, since N elements need to be moved,
  the entire process takes O(N) time.
-}

-- C.4
{-
  reverse [1, 2, 3]
  --> reverse [2, 3] ++ [1] [from definition]
  --> (reverse [3] ++ [2]) ++ [1]
  --> ((reverse [] ++ [3]) ++ [2]) ++ [1]
  --> (([] ++ [3]) ++ [2]) ++ [1]   (***)
  --> ([3] ++ [2]) ++ [1]  
  --> (3 : ([] ++ [2])) ++ [1]
  --> (3 : [2]) ++ [1]
  --> [3, 2] ++ [1]
  --> 3 : ([2] ++ [1])
  --> 3 : (2 : ([] ++ [1]))
  --> 3 : (2 : [1])
  --> 3 : [2, 1]
  --> [3, 2, 1]
 
  The asymptotic complexity of this version of reverse is O(N^2).
  After reaching step (***) in the evaluation, which takes a linear number
  of steps, it takes O(N^2) time to construct the list. This is because
  the list concatentation operator is an O(N) operation (it processes every
  element in its first argument), and we use this operator N times on lists
  of up to size (N - 1).
-}

 -- C.5
{-
  head (isort [3, 1, 2, 5, 4])
  --> head (insert 3 (isort [1, 2, 5, 4])) [from definition]
  --> head (insert 3 (insert 1 (isort [2, 5, 4])))
  --> head (insert 3 (insert 1 (insert 2 (isort [5, 4]))))
  --> head (insert 3 (insert 1 (insert 2 (insert 5 (isort [4])))))
  --> head (insert 3 (insert 1 (insert 2 (insert 5 (insert 4 (isort []))))))
  --> head (insert 3 (insert 1 (insert 2 (insert 5 (insert 4 [])))))
  --> head (insert 3 (insert 1 (insert 2 (insert 5 [4]))))
  --> head (insert 3 (insert 1 (insert 2 (4 : insert 5 []))))
  --> head (insert 3 (insert 1 (2 : 4 : insert 5 [])))
  --> head (insert 3 (1 : 2 : 4 : insert 5 []))
  --> head (1 : insert 3 (2 : 4 : insert 5 []))
  --> 1
-}

-- C.6
{- 
  foldr max 0 [1, 5, 3, -2, 4]
  --> max 1 (foldr max 0 [5, 3, -2, 4]) [from definition]
  --> max 1 (max 5 (foldr max 0 [3, -2, 4]))
  --> max 1 (max 5 (max 3 (foldr max 0 [-2, 4])))
  --> max 1 (max 5 (max 3 (max -2 (foldr max 0 [4]))))
  --> max 1 (max 5 (max 3 (max -2 (max 4 (foldr max 0 [])))))
  --> max 1 (max 5 (max 3 (max -2 (max 4 0)))) (***)
  --> max 1 (max 5 (max 3 (max -2 4)))
  --> max 1 (max 5 (max 3 4))
  --> max 1 (max 5 4)
  --> max 1 5
  --> 5
 
  foldl max 0 [1, 5, 3, -2, 4]
  --> foldl max (max 0 1) [5, 3, -2, 4] [from definition]
  --> foldl max ((max 0 1) 5) [3, -2, 4]
  --> foldl max (max (max (max 0 1) 5) 3) [-2, 4]
  --> foldl max (max (max (max (max 0 1) 5) 3) -2) [4]
  --> foldl max (max (max (max (max (max 0 1) 5) 3) -2) 4) []
  --> max (max (max (max (max 0 1) 5) 3) -2) 4 (***)
  --> max (max (max (max 1 5) 3) -2) 4
  --> max (max (max 5 3) -2) 4
  --> max (max 5 -2) 4
  --> max 5 4
  --> 5
 
  The space complexity of foldr and foldl is the same. Since haskell has
  lazy evaluation, both foldr and foldl perform full evaluations before
  any reductions occur. Thus, there are equal amounts of "thunks", or
  values that have yet to be evaluated, by the time reduction actually
  starts to happen. This can be seen above; in the lines labeled (***),
  five "max" functions still need to be evaluated, with their corresponding
  arguments. Note that, if foldl was strict in its "init" argument, it would
  be more space efficient.
-}
