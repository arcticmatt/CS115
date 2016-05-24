import Control.Monad

-- ****************************************
-- ********** Part A **********************
-- ****************************************

-- A.1
-- hr_solutions :: [((Integer, Integer), (Integer, Integer), Integer)]
-- hr_solutions =
--   [((i, l), (j, k), i^3 + l^3) | 
--    i <- [1..], 
--    j <- [1..i-1], 
--    k <- [1..j-1], 
--    l <- [1..k-1], 
--    i^3 + l^3 == j^3 + k^3]
--
-- We will use the list monad to find positive integers that can be expressed 
-- as the sum of two cubes in two different ways. The list comprehension 
-- code can be seen above.
hr_solutions :: [((Integer, Integer), (Integer, Integer), Integer)]
hr_solutions = 
  do i <- [1..]
     j <- [1..i-1]
     k <- [1..j-1]
     l <- [1..k-1]
     guard $ i^3 + l^3 == j^3 + k^3
     return ((i, l), (j, k), i^3 + l^3)

-- A.2
-- Using the list monad, we will write an expression which computes the sum of 
-- the natural numbers below one thousand which are multiples of 3 or 5.
-- We will first do this using the guard function.
nSum1 :: Integer
nSum1 = sum lst
  where
    lst :: [Integer]
    lst = do x <- [0..999]
             -- If boolean is true, x is returned, otherwise case is wiped out.
             guard $ x `mod` 3 == 0 || x `mod` 5 == 0 
             return x

-- Next, we will do this without guard. Instead, we will use mzero from the 
-- MonadPlus instance.
nSum2 :: Integer
nSum2 = sum lst
  where
    lst :: [Integer]
    lst = do x <- [0..999]
             if x `mod` 3 == 0 || x `mod` 5 == 0 
               then return x -- Return all desired values.
               else mzero

-- A.3
-- Takes an Integer and returns True if the integer's decimal representation 
-- is a palindrome. Does so by converting the Integer to a String, and comparing
-- the String to its reversed form.
isPalindrome :: Integer -> Bool
isPalindrome x = str == reverse str where str = show x

-- Finds the largest palindrome made from the product of two 3-digit numbers.
-- Does so by using the list monad. 
--
-- The pseudocode is as follows:
-- Generate all pairs of three digit numbers
-- e.g. 
-- x [100 - 999]
-- y [100 - 999]
-- (x, y)
-- Multiply them
-- See if the product s a palindrome
-- Take max of resulting list
largestPalindrome :: Integer
largestPalindrome = maximum lst
  where 
    lst :: [Integer]
    lst = do x <- [100..999]
             y <- [100..999]
             guard $ isPalindrome $ x * y
             return $ x * y

-- The answer is 906609

-- A.4
-- Here is the code that was given to us.
-- Datatypes to represent expressions.
-- An expression, represented by a list of items.
type Expr = [Item]

-- An item can be either an int or an operation.
data Item = N Int | O Op
  deriving Show

-- We have three operations: addition, subtraction, and concatentation.
data Op = Add | Sub | Cat
  deriving Show

-- List of all operators.
ops :: [Item]
ops = [O Add, O Sub, O Cat]

-- Pick out the expressions that evaluate to a particular number.
find :: Int -> [Expr] -> [Expr]
find n = filter (\e -> evaluate (normalize e) == n)

-- Pretty-print an expression.
pprint :: Expr -> String
pprint [N i] = show i
pprint (N i : O Add : es) = show i ++ " + " ++ pprint es
pprint (N i : O Sub : es) = show i ++ " - " ++ pprint es
pprint (N i : O Cat : es) = show i ++ pprint es
pprint _ = error "pprint: invalid argument"

-- Run the computation and print out the answers.
run :: IO ()
run = mapM_ putStrLn $ map pprint $ find 100 exprs

-- The puzzle we are trying to solve is as follows:
-- Take the digits 1 to 9 in sequence. Put either a "+", a "-", or nothing 
-- between each digit to get an arithmetic expression. Print out all such 
-- expressions that evaluate to 100.
--
-- exprs is a list of all possible valid expressions from the puzzle description,
-- i.e. all possible combinations of the digits 1 to 9 (in order) with 
-- one of the operators from the Op datatype between each digit. We will 
-- use the list monad to generate these expressions.
exprs :: [Expr]
exprs =
  do op1 <- ops
     op2 <- ops
     op3 <- ops
     op4 <- ops
     op5 <- ops
     op6 <- ops
     op7 <- ops
     op8 <- ops
     return [N 1, op1, N 2, op2, N 3, op3, N 4, op4, N 5, op5, N 6, op6, N 7, 
             op7, N 8, op8, N 9]

-- Concatenates two ints. E.g. concatInts 4 5 = 45.
concatInts :: Int -> Int -> Int 
concatInts x y = read $ show x ++ show y :: Int

-- This function takes an expression and removes all instances of the Cat 
-- operator by applying the transformation
-- N i, Cat, N j --> N (ij) anywhere in the list.
-- It also throws errors upon seeing illegitmate patterns (e.g. multiple 
-- operators/numbers in a row or expressions that begin or end with operators).
--
-- The basic idea is to pattern match all valid subexpressions, and make 
-- everything else an error.
normalize :: Expr -> Expr
normalize [] = []
normalize ((N x):[]) = [N x]
normalize ((N x):(O Cat):(N y):xs) = normalize (N (concatInts x y) : xs)
normalize ((N x):(O Add):(N y):xs) = [N x, O Add] ++ normalize (N y : xs)
normalize ((N x):(O Sub):(N y):xs) = [N x, O Sub] ++ normalize (N y : xs)
normalize _ = error "normalize error: expression contains an illegimate pattern"

-- This function takes a normalized expression (i.e. one with no Cat operators)
-- and evaluates it to give an Int. Since subtraction associates to the left,
-- we start evaluating from the beggining of the expresssion towards the 
-- end, not the other way around.
evaluate :: Expr -> Int 
evaluate ((N x):(O Add):(N y):xs) = evaluate (N (x + y) : xs)
evaluate ((N x):(O Sub):(N y):xs) = evaluate (N (x - y) : xs)
evaluate ((N x):[]) = x
evaluate [] = 0
evaluate _ = error "evaluate error: expression contains an illegitmate pattern"

-- ****************************************
-- ********** Part B **********************
-- ****************************************

-- B.1
-- do n1 <- [1..6]
--   n2 <- [1..6]
--   []
--   return (n1, n2)
--
-- The above desugars to...
--
-- [1..6] >>= \n1 -> 
--   [1..6] >>= \n2 -> 
--     [] >> 
--       return (n1, n2)
-- 
-- We then substitute the definition of >> to derive the following.
--
-- [1..6] >>= \n1 -> 
--   [1..6] >>= \n2 -> 
--     [] >>= \_ ->
--       return (n1, n2)
-- 
-- And then substitute the definition of >>= (the one that uses concatMap).
--
-- [1..6] >>= \n1 -> 
--   [1..6] >>= \n2 -> 
--     concatMap (\_ -> return (n1, n2)) [] 
--
-- [1..6] >>= \n1 -> 
--   concatMap (\n2 -> (concatMap (\_ -> return (n1, n2)) [])) [1..6]
--
-- concatMap (\n1 -> 
--   (concatMap (\n2 -> (concatMap (\_ -> return (n1, n2)) [])) [1..6])) [1..6]
--
-- Note that concatMap <anything> [] is the empty list. Thus,
-- 
-- concatMap (\n1 -> (concatMap (\n2 -> []) [1..6])) [1..6]
--
-- concatMap (\n1 -> (concat (map (\n2 -> []) [1..6]))) [1..6] *
--
-- concatMap (\n1 -> (concat ([[], [], ..., []]))) [1..6]      **
--
-- concatMap (\n1 -> []) [1..6]
--
-- []  -- Following same logic as * and **, as seen above.

-- B.2
-- Let's first reduce the following expression:
-- do n1 <- [1..6]
--    n2 <- [1..6]
--    return <anything>
--    return (n1, n2)
-- 
-- The above desugars to...
--
-- [1..6] >>= \n1 ->
--   [1..6] >>= \n2 ->
--     return <anything> >>
--       return (n1, n2)
-- 
-- We then substitute the definition of >> to derive the following.
--
-- [1..6] >>= \n1 ->
--   [1..6] >>= \n2 ->
--     return <anything> >>= \_ ->
--       return (n1, n2)
--
--  And then substitute the definition of >>=.
-- 
-- [1..6] >>= \n1 ->
--   [1..6] >>= \n2 ->
--     concatMap (\_ -> return (n1, n2)) return <anything>
-- 
-- [1..6] >>= \n1 ->
--   [1..6] >>= \n2 ->
--     concat (map (\_ -> return (n1, n2)) return <anything>)
--
-- [1..6] >>= \n1 ->
--   [1..6] >>= \n2 ->
--     concat (map (\_ -> return (n1, n2)) return <anything>)
--
-- [1..6] >>= \n1 ->
--   [1..6] >>= \n2 ->
--     concat (map (\_ -> return (n1, n2)) [anything])
--
-- [1..6] >>= \n1 ->
--   [1..6] >>= \n2 ->
--     concat ([return (n1, n2)])
-- 
-- [1..6] >>= \n1 ->            (***)
--   [1..6] >>= \n2 ->
--     return (n1, n2)
-- 
-- Now we can reduce the other expression:
-- do n1 <- [1..6]
--    n2 <- [1..6]
--    return (n1, n2)
-- 
-- This desugars to...
--
-- [1..6] >>= \n1 ->            (***)
--   [1..6] >>= \n2 ->
--     return (n1, n1)
-- 
-- which is equivalent to the reduced version of the other expression.

-- B.3
-- let s = ["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"] in
--   do ['a', 'a', c1, c2, 'b', 'b'] <- s 
--      return [c1, c2]
-- 
-- We will desugar this using the full case-style desugaring of do expressions
-- covered in lecture 11.
--
-- do ['a', 'a', c1, c2, 'b', 'b'] <- ["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"]
--   return [c1, c2]
--
-- ["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"] >>= 
--   \x -> case x of 
--     ('a':'a':c1:c2:'b':'b') -> return [c1, c2]
--     _ -> fail "Pattern match failure in do expression"
--
-- concatMap (\x -> case x of 
--             ('a':'a':c1:c2:'b':'b') -> return [c1, c2]
--             _ -> fail "Pattern match failure in do expression")
--           ["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"]         
-- 
-- concat (map (\x -> case x of 
--               ('a':'a':c1:c2:'b':'b') -> return [c1, c2]
--               _ -> fail "Pattern match failure in do expression")
--             ["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"])
--
-- Note we will use syntactic sugaring here, representing list of Chars as
-- Strings.
--
-- concat ([return "xy", return "zw", [], return "cc", []])
--
-- ["xy", "zw", "cc"]
--
-- 
-- Let's consider what would happen if fail for the list monad looked like 
-- fail s = error s.
--
-- If this were the case, then upon mapping the function over the first 
-- value that did not contain the desired pattern, and error would be 
-- thrown and the expression would terminate. Note that, since Haskell
-- is lazily evaluated, all valid values that appeared beforehand would 
-- be properly evaluated.

-- B.4 
-- Let's first evaluate the following expression where m = [x1, x2, ...]
--
-- m >>= k = foldr ((++) . k) [] m
-- foldr ((++) . k) [] [x1, x2, ...]
-- foldr (\x -> (++) (k x)) [] [x1, x2, ...]
-- foldr (\x -> (++) (k x)) [] [x1, x2, ...]
-- foldr (\x -> (++) (k x)) [] [x1, x2, ...]
-- (++) (k x1) foldr [x2, ...]
-- (k x1) ++ foldr [x2, ...]
-- (k x1) ++ (k x2) ++ foldr [x3, ...]
-- ... etc
-- (k x1) ++ (k x2) ++ (k x3) ++ ...
--
-- Now we'll evaluate the same expression when m = []. This is fairly trivial
-- given the definition of foldr.
-- foldr ((++) . k) [] []
-- []
--
-- Now let's evaluate the other expression where m = [x1, x2, ...]
-- m >>= k = concat (map k m)
-- concat (map k [x1, x2, ..])
-- concat ([(k x1), (k x2), ...])
-- concat ([(k x1), (k x2), ...])
--
-- Using the definition of concat from lec. 5
-- concat [] = []
-- concat (xs:xss) = xs ++ (concat xss)
-- we get the following
--
-- (k x1) ++ concat [(k x2), ...])
-- (k x1) ++ (k x2) ++ concat [(k x3), ...]
-- ... etc
-- (k x1) ++ (k x2) ++ (k x3) + ...
--
-- Nowe we'll evaluate the same expression when m = []. This is fairly trivial
-- given the definition of concat.
-- concat (map k [])
-- concat []
-- []
--
-- Thus, we can see that these two expressions do indeed compute the same
-- thing.

-- B.5
-- The problem with the code is that, on line 11, it tries to add together
-- two Nums. However, these two Nums do not have defined types. That is,
-- it is ambiguous if "n" and "s" are the same type, the same instance 
-- of Num. If they are not, then there is no defined way to add them
-- together. This is why we get the "cannot deduce type equality" error;
-- while we know that both "n" and "s" must be instances of the type class
-- Num, we never ensure that they are the SAME instance. In other words,
-- we never check for type equality, which means that we cannot safely
-- perform the addition. 
--
-- This problem does not have a definitive solution. We could choose a Num 
-- type instance to cast every number in the list to, and add all the 
-- casted numbers. For example, we could use a combination of show and read
-- to cast every Num to a float (as long as every Num instance had a well-defined
-- show function), and return a float as the final sum. This would ensure that
-- the sum kept precision. 
--
-- In general, there is not a great solution. Since the datatype allows for 
-- all Num instances, and the (+) operator operates on the same Num instance,
-- it is not clear how the addition should work at each step with regards 
-- to typing, rounding, and truncation. Further, without a forced casting 
-- pattern as mentioned above, we run into compilation errors as seen
-- in the assignment.
