module Lab3ab
where

-- ****************************************
-- ********** Part A **********************
-- ****************************************

-- Needed for Part A.
data Nat = Zero | Succ Nat 
           deriving (Eq, Show)

-- A.1
-- Manual definitions of instances of the Eq and Show type classes for Nat.
{-
instance Eq Nat where 
    Zero == Zero         = True
    Zero == _            = False
    _ == Zero            = False
    (Succ x) == (Succ y) = (x == y)
    x /= y               = not (x == y)

instance Show Nat where 
    show Zero        = "Zero"
    show (Succ Zero) = "Succ Zero"
    show (Succ x)    = "Succ (" ++ show x ++ ")"
-}

-- A.2
-- Have Haskell derive the Eq and Show instances for Nat automatically.
-- For this, see the data declaration at the top of the file.

-- A.3
-- An explicit instance definition of the Ord type class for the Nat type. 
-- Note that we only define the <= operator, since all other Ord methods 
-- can be defined in terms of <=. 
--
-- We could have had Haskell derive the Ord instance for us. This is because,
-- when Haskell derives Ord, the values denoted by earlier constructors
-- are considered smaller. In our case, this means Zero is less than (Succ Nat).
-- Then, when comparing two (Succ Nat) values, it goes on to compare what's 
-- inside them (the value constructor values). This comparison is also as 
-- desired.
instance Ord Nat where 
    Zero <= Zero         = True
    Zero <= _            = True
    _ <= Zero            = False 
    (Succ x) <= (Succ y) = (x <= y)

-- A.4
-- Manual definitions of instances of the Eq and Show type classes for 
-- SignedNat.
--
-- We cannot use automatically-derived definitions for the Eq and Ord instances.
-- These would be incorrect when comparing (Neg Zero) and (Pos Zero); they 
-- should be equal, but Haskell would look at the different value constructors 
-- and say otherwise.
data SignedNat =
  Neg Nat | Pos Nat
  deriving (Show)

instance Eq SignedNat where 
    (Neg Zero) == (Pos Zero) = True -- Handle flipped signs
    (Neg _) == (Pos _)       = False 
    (Pos Zero) == (Neg Zero) = True
    (Pos _) == (Neg _)       = False 
    (Neg x) == (Neg y)       = (x == y) -- Handle same signs
    (Pos x) == (Pos y)       = (x == y)

instance Ord SignedNat where 
    (Neg _) <= (Pos _)       = True -- Handle Neg/Pos cases
    (Pos Zero) <= (Neg Zero) = True -- Handle Pos/Neg cases
    (Pos _) <= (Neg _)       = False 
    (Pos x) <= (Pos y)       = (x <= y) -- Handle ambiguous cases
    (Neg x) <= (Neg y)       = (x == y) || (x > y)

-- A.5
-- The Num instance definition for SignedNat.
-- We will define all the methods as stand-alone methods.
-- First, we'll define helper functions to do arithmetic operations on Nats.

-- Adds two Nat instances.
addNat :: Nat -> Nat -> Nat 
addNat Zero Zero         = Zero
addNat Zero n@(Succ _)   = n
addNat n@(Succ _) Zero   = n
addNat (Succ x) (Succ y) = Succ (Succ (addNat x y))

-- Subtracts two Nat instances. The second argument must be less than or equal 
-- to the first. We do subtraction of two numbers x and y (where y <= x) by 
-- continually "subtracting" one from each number until y is 0. By this point,
-- x is our desired result. That is, consider 5 - 2. This reduces down to 3 - 0,
-- making the answer 3.
subNat :: Nat -> Nat -> Nat 
subNat Zero Zero       = Zero
subNat n@(Succ _) Zero = n
subNat Zero _          = error "subNat: second arg greater than first"
subNat (Succ x) (Succ y) | y > x     = error "subNat: second arg greater than first"
                         | y == x    = Zero
                         | otherwise = subNat x y

-- Multiplies two Nat instances. To do the multiplication of two Nat instances
-- x and y, add the Nat instance x "y" number of times. That is, if x = 5 
-- and y = 3, mulNat is equivalent to 5 + 5 + 5.
mulNat :: Nat -> Nat -> Nat 
mulNat _ Zero = Zero 
mulNat Zero _ = Zero
mulNat n@(Succ _) (Succ y) = addNat n (mulNat n y)

-- Converts an integer into a Nat instance.
natFromInteger :: Integer -> Nat
natFromInteger 0 = Zero
natFromInteger x | x > 0     = Succ (natFromInteger (x - 1))
                 | otherwise = Succ (natFromInteger (x + 1))

-- Adds two SignedNat instances. When adding a positive and negative number,
-- subtract the number which is lower in absolute value from the other number,
-- and then give it the correct sign.
addSignedNat :: SignedNat -> SignedNat -> SignedNat 
addSignedNat (Pos x) (Pos y) = Pos (addNat x y)
addSignedNat (Neg x) (Neg y) = Neg (addNat x y)
addSignedNat (Pos x) (Neg y) | x > y     = Pos (subNat x y) -- e.g. 5 + (-4)
                             | otherwise = Neg (subNat y x) -- e.g. 5 + (-8)
addSignedNat (Neg y) (Pos x) | x > y     = Pos (subNat x y) -- same as above 2 cases
                             | otherwise = Neg (subNat y x) 

-- Multiplies two SignedNat instances. Gives signs in the usual way (pos * pos 
-- is pos, neg * pos is neg, neg * neg is pos).
mulSignedNat :: SignedNat -> SignedNat -> SignedNat
mulSignedNat (Pos x) (Pos y) = Pos (mulNat x y)
mulSignedNat (Neg x) (Neg y) = Pos (mulNat x y)
mulSignedNat (Pos x) (Neg y) = Neg (mulNat x y)
mulSignedNat (Neg y) (Pos x) = Neg (mulNat x y)

-- Subtracts two SignedNat instances.
-- Note: Have results that can end up in Zero be represented by (Pos Zero).
subSignedNat :: SignedNat -> SignedNat -> SignedNat 
subSignedNat (Neg x) (Pos y) = Neg (addNat x y) -- e.g. -5 - 7 <=> -(5 + 7)
subSignedNat (Pos x) (Neg y) = Pos (addNat x y) -- e.g. 5 - (-7)
subSignedNat (Pos x) (Pos y) | x >= y    = Pos (subNat x y) -- 1st arg pos,
                             | otherwise = Neg (subNat y x) -- 2nd arg neg
subSignedNat (Neg x) (Neg y) | x > y     = Neg (subNat x y) -- Here, the 2nd 
                             | otherwise = Pos (subNat y x) -- arg turns pos

-- Negates a SignedNat instance. Uses subSignedNat.
negateNat :: SignedNat -> SignedNat 
negateNat n@(Pos _) = subSignedNat (Pos Zero) n
negateNat n@(Neg _) = subSignedNat (Pos Zero) n

-- Returns the absolute value of a SignedNat instance, as a SignedNat.
absNat :: SignedNat -> SignedNat 
absNat n@(Pos _) = n
absNat (Neg x) = Pos x

-- Returns the sign of a SignedNat instance; 1 for positive, -1 for negative,
-- 0 for 0.
signumNat :: SignedNat -> SignedNat
signumNat (Pos Zero) = Pos Zero     -- 0
signumNat (Neg Zero) = Pos Zero     -- 0
signumNat (Pos _) = Pos (Succ Zero) -- 1
signumNat (Neg _) = Neg (Succ Zero) -- -1

-- Converts an integer into a SignedNat instance.
signedNatFromInteger :: Integer -> SignedNat
signedNatFromInteger 0 = Pos Zero
signedNatFromInteger x | x > 0     = Pos (natFromInteger x)
                       | otherwise = Neg (natFromInteger x)

-- An instance of the Num type class for SignedNat. Relies entirely of the 
-- helper functions declared above.
instance Num SignedNat where 
    (+) = addSignedNat
    (-) = subSignedNat
    (*) = mulSignedNat
    negate = negateNat 
    abs = absNat 
    signum = signumNat 
    fromInteger = signedNatFromInteger

-- A.6
-- A version of foldr that works on Nats. It specifies a special value to 
-- be used in place of Zero, and a special unary function to be used in place 
-- of Succ.
foldn :: (a -> a) -> a -> Nat -> a
foldn _ init Zero = init
foldn f init (Succ n) = f (foldn f init n)

-- Converts a Nat instance to an integer. Used for SignedNatToInteger.
natToInteger :: Nat -> Integer
natToInteger = foldn (1+) 0

-- Converts a SignedNat instance to an integer. Uses natToInteger.
signedNatToInteger :: SignedNat -> Integer
signedNatToInteger (Pos x) = natToInteger x
signedNatToInteger (Neg x) = -(natToInteger x)

-- A.7
-- One thing that's redundant about SignedNat is that it allows for both 
-- (Pos Zero) and (Neg Zero). Thus, when patten matching on SignedNat 
-- instances, it is necessary to check for both possibilities, leading 
-- to more definitions than one would like. To get rid of this redundancy,
-- we can switch to a unary integer representation, which doesn't have 
-- an explicit symbol for zero. Then, we can represent zero with the "Nothing"
-- value. In order to do this programmatically, we can use the Maybe typeclass.
-- That is, functions will not take in our data type directly. Instead, they 
-- will wrap it with a "Maybe", indicating that the value can either be a 
-- positive or negative integer, or zero (indicated by Nothing).
--
-- An alternative solution would be to add the "Zero" value constructor into 
-- the SignedUnaryInteger type (but still keeping it out of UnaryInteger).
--
-- Both these solutions still have problems. Using Maybes everywhere would add
-- unwanted clutter to the code. Further, both these solutions do nothing to 
-- cut down on the datatype's large space complexity (i.e. space for these 
-- instances scales linearly with the number).
--
-- To fix this, we could implement a binary data type, where we represent 
-- numbers with strings of 1s and 0s. This would greatly improve the space 
-- complexity of our data type. 

-- Data declaration for the unsigned UnaryInteger. The base is one; numbers
-- greater than one are represented by a succession of ones.
data UnaryInteger = One | USucc UnaryInteger  
                    deriving (Eq, Show)

-- Data declaration for the signed UnaryInteger. Simply adds a sign to 
-- the UnaryInteger type.
data SignedUnaryInteger = UNeg UnaryInteger | UPos UnaryInteger
                          deriving (Show)

-- A.8
factorial :: (Num a, Ord a) => a -> a
factorial 0 = 1
factorial n | n < 0     = error "factorial has a negative argument"
            | otherwise = n * factorial (n - 1)
-- Pos (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))

-- ****************************************
-- ********** Part B **********************
-- ****************************************

-- B.1
-- B.1.1
-- Here, we consider the >#< operator. It is
-- infix 
-- because chained operator expressions lead to type errors.
-- For example, 
-- (51 >#< 40) >#< 30 
-- => "First Player" >#< 30

-- B.1.2
-- Here, we consider the +| operator. It is 
-- infixl
-- but it could also be infixr. 
-- Here's an example of a chained operator expression:
-- 7 +| 6 +| 5 
-- => (7 +| 6) +| 5 
-- => 3 |+ 5 => 8

-- B.1.3
-- Here, we consider the &< operator. It is 
-- infixl.
-- Here's an example of a chained operator expression.
-- [1 ,2] &< 3 &< 4 
-- => ([1 ,2] &< 3) &< 4 
-- => [1, 2, 3] &< 4 
-- => [1, 2, 3, 4]

-- B.1.4
-- Here, we consider the >&& operator. It is 
-- infixr.
-- Here's an example of a chained operator expression.
-- 1 >&& 2 >&& [3, 4] 
-- => 1 >&& (2 >&& [3, 4]) 
-- => 1 >&& [2, 2, 3, 4]
-- => [1, 1, 2, 2, 3, 4]

-- B.2 
-- Here, we consider the +# operator.
--
-- In order for chained operators to type check, its associativity could 
-- be infixr, infixl, or infix (infix would require the programmer to explicitly
-- put parentheses). 
--
-- However, for correctness, its associativity should be infix. This is because
-- the correct parenthesization for chained operators is ambiguous. In other 
-- words, chained operations cannot always be correctly parenthesized by 
-- simply grouping operators in a left-associative or right-associative 
-- manner. Here's an example.
-- 2 +# 10 +# 1000
-- The final sum, 1012, has four digits. Let's consider the infixr result.
-- 2 +# (10 +# 1000)
-- 2 +# 4
-- 1
-- So in this case, infixr gives the wrong results. Let's consider the same
-- numbers arranged in a different order, and look at the results which 
-- infixl would give.
-- 10 +# 1000 +# 2
-- (10 +# 1000) +# 2
-- 4 +# 2
-- 1
-- Thus, it is evident that infix associativity is needed in order to 
-- "smartly" parenthesize the expression.
--
-- Note, however, that even infix associativy does not guarantee that this 
-- operator will always act as desired. Consider the following example:
-- 2 +# 700 +# 333 
-- The sensible result for this operation would be 4; this number would be 
-- obtained by adding all the numbers together (to get 1035), and then taking
-- the number of digits of the sum. However, by parenthesizing this 
-- expression, we get different results. Let's see how this looks:
-- (2 +# 700) +# 333
-- 3 +# 333
-- 3
-- or
-- 2 +# (700 +# 333)
-- 2 +# 4
-- 1
-- Either way we parenthesize this expression leads to an undesired result. 
-- That is, even if we give the +# operator infix associativity, correct results 
-- are not always guaranteed. 
--
-- Thus, we can conclude that, given its unreliable behavior, this operator 
-- should not necessarily even BE an operator. However, if one does decide to make
-- it an operator, infix associativity is the way to go.
