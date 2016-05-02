--
-- SignedNatTests.hs
--
-- Tests of signed natural number datatype.
--

{-# LANGUAGE TemplateHaskell #-}

module SignedNatTests where

import Test.QuickCheck
-- Apparently not needed for GHC 7.8:
-- import Test.QuickCheck.All

import Lab3ab

----------------------------------------------------------------------
-- Helper functions.
----------------------------------------------------------------------

make_prop_eq :: Eq a => (Integer -> a) -> (Integer, Integer) -> Property
make_prop_eq fromint (imin, imax) = do
  forAll twoIntegers check
  where
    twoIntegers :: Gen (Integer, Integer)
    twoIntegers = do
      i <- choose (imin, imax)
      j <- choose (imin, imax)
      return (i, j)

    check :: (Integer, Integer) -> Bool
    check (i, j) = 
      let
        k  = i == j
        i' = fromint i
        j' = fromint j
        k' = i' == j'
      in k' == k

make_prop_eq_with_first :: Eq a => 
  (Integer -> a) -> (Integer, Integer) -> a -> Integer -> Property
make_prop_eq_with_first fromint (imin, imax) first firstint = do
  forAll anInteger check
  where
    anInteger :: Gen Integer
    anInteger = do
      i <- choose (imin, imax)
      return i

    check :: Integer -> Bool
    check i = 
      let
        j  = firstint == i
        i' = fromint i
        j' = first == i'
      in j' == j

make_prop_eq_with_second :: Eq a => 
  (Integer -> a) -> (Integer, Integer) -> a -> Integer -> Property
make_prop_eq_with_second fromint (imin, imax) second secondint = do
  forAll anInteger check
  where
    anInteger :: Gen Integer
    anInteger = do
      i <- choose (imin, imax)
      return i

    check :: Integer -> Bool
    check i = 
      let
        j  = i == secondint
        i' = fromint i
        j' = i' == second
      in j' == j

make_prop_le :: Ord a => (Integer -> a) -> (Integer, Integer) -> Property
make_prop_le fromint (imin, imax) = do
  forAll twoIntegers check
  where
    twoIntegers :: Gen (Integer, Integer)
    twoIntegers = do
      i <- choose (imin, imax)
      j <- choose (imin, imax)
      return (i, j)

    check :: (Integer, Integer) -> Bool
    check (i, j) = 
      let
        k  = i <= j
        i' = fromint i
        j' = fromint j
        k' = i' <= j'
      in k' == k

make_prop_le_with_first :: Ord a => 
  (Integer -> a) -> (Integer, Integer) -> a -> Integer -> Property
make_prop_le_with_first fromint (imin, imax) first firstint = do
  forAll anInteger check
  where
    anInteger :: Gen Integer
    anInteger = do
      i <- choose (imin, imax)
      return i

    check :: Integer -> Bool
    check i = 
      let
        j  = firstint <= i
        i' = fromint i
        j' = first <= i'
      in j' == j

make_prop_le_with_second :: Ord a => 
  (Integer -> a) -> (Integer, Integer) -> a -> Integer -> Property
make_prop_le_with_second fromint (imin, imax) second secondint = do
  forAll anInteger check
  where
    anInteger :: Gen Integer
    anInteger = do
      i <- choose (imin, imax)
      return i

    check :: Integer -> Bool
    check i = 
      let
        j  = i <= secondint
        i' = fromint i
        j' = i' <= second
      in j' == j

-- Test that converting int->nat->int gives the same number back.
make_prop_convert :: Num a => (a -> Integer) -> (Integer, Integer) -> Property
make_prop_convert toint (imin, imax) = do
  forAll anInteger check
  where
    anInteger :: Gen Integer
    anInteger = do
      i <- choose (imin, imax)
      return i

    check :: Integer -> Bool
    check i = toint (fromInteger i) == i

-- Test that a unary function on a numeric type gives the same results as a
-- unary function on Integers.
make_prop_unary_op :: Num a =>
  (a -> a) -> (Integer -> Integer) -> (a -> Integer) -> 
  (Integer, Integer) -> Property
make_prop_unary_op f fint toint (imin, imax) = do
  forAll anInteger check
  where
    anInteger :: Gen Integer
    anInteger = do
      i <- choose (imin, imax)
      return i

    check :: Integer -> Bool
    check i = 
      let
        j  = fint i
        i' = fromInteger i
        j' = f i'
      in toint j' == j

-- Test that a binary function on a numeric type gives the same results as a
-- binary function on Integers.
make_prop_binary_op :: Num a =>
  (a -> a -> a) -> (Integer -> Integer -> Integer) -> (a -> Integer) -> 
  (Integer, Integer) -> Property
make_prop_binary_op f fint toint (imin, imax) = do
  forAll twoIntegers check
  where
    twoIntegers :: Gen (Integer, Integer)
    twoIntegers = do
      i <- choose (imin, imax)
      j <- choose (imin, imax)
      return (i, j)

    check :: (Integer, Integer) -> Bool
    check (i, j) = 
      let
        k  = fint i j
        i' = fromInteger i
        j' = fromInteger j
        k' = f i' j'
      in toint k' == k

-- The same as above, but supplying a fixed argument for the first argument
-- to the binary operator.
make_prop_binary_op_with_first :: Num a =>
  (a -> a -> a) -> (Integer -> Integer -> Integer) -> (a -> Integer) -> 
  (Integer, Integer) -> a -> Integer -> Property
make_prop_binary_op_with_first f fint toint (imin, imax) first firstint = do
  forAll anInteger check
  where
    anInteger :: Gen Integer
    anInteger = do
      i <- choose (imin, imax)
      return i

    check :: Integer -> Bool
    check i = 
      let
        j  = fint firstint i
        i' = fromInteger i
        j' = f first i'
      in toint j' == j

-- The same as above, but supplying a fixed argument for the first argument
-- to the binary operator.
make_prop_binary_op_with_second :: Num a =>
  (a -> a -> a) -> (Integer -> Integer -> Integer) -> (a -> Integer) -> 
  (Integer, Integer) -> a -> Integer -> Property
make_prop_binary_op_with_second f fint toint (imin, imax) second secondint = do
  forAll anInteger check
  where
    anInteger :: Gen Integer
    anInteger = do
      i <- choose (imin, imax)
      return i

    check :: Integer -> Bool
    check i = 
      let
        j  = fint i secondint
        i' = fromInteger i
        j' = f i' second
      in toint j' == j

----------------------------------------------------------------------
-- Properties to test.
-- NOTE: These depend on signedNatToInteger, which may not depend
-- on either Neg Zero or Pos Zero, so we test specifically for these.
----------------------------------------------------------------------

prop_eq :: Property
prop_eq =
  make_prop_eq (fromInteger :: Integer -> SignedNat) (-10, 10)

prop_eq_neg_zero_first :: Property
prop_eq_neg_zero_first =
  make_prop_eq_with_first (fromInteger :: Integer -> SignedNat) (-10, 10)
    (Neg Zero) 0

prop_eq_neg_zero_second :: Property
prop_eq_neg_zero_second =
  make_prop_eq_with_second (fromInteger :: Integer -> SignedNat) (-10, 10)
    (Neg Zero) 0

prop_eq_pos_zero_first :: Property
prop_eq_pos_zero_first =
  make_prop_eq_with_first (fromInteger :: Integer -> SignedNat) (-10, 10)
    (Pos Zero) 0

prop_eq_pos_zero_second :: Property
prop_eq_pos_zero_second =
  make_prop_eq_with_second (fromInteger :: Integer -> SignedNat) (-10, 10)
    (Pos Zero) 0


prop_le :: Property
prop_le =
  make_prop_le (fromInteger :: Integer -> SignedNat) (-10, 10)

prop_le_neg_zero_first :: Property
prop_le_neg_zero_first =
  make_prop_le_with_first (fromInteger :: Integer -> SignedNat) (-10, 10)
    (Neg Zero) 0

prop_le_neg_zero_second :: Property
prop_le_neg_zero_second =
  make_prop_le_with_second (fromInteger :: Integer -> SignedNat) (-10, 10)
    (Neg Zero) 0

prop_le_pos_zero_first :: Property
prop_le_pos_zero_first =
  make_prop_le_with_first (fromInteger :: Integer -> SignedNat) (-10, 10)
    (Pos Zero) 0

prop_le_pos_zero_second :: Property
prop_le_pos_zero_second =
  make_prop_le_with_second (fromInteger :: Integer -> SignedNat) (-10, 10)
    (Pos Zero) 0


prop_plus :: Property
prop_plus = 
  make_prop_binary_op (+) (+) signedNatToInteger (-50, 50)

prop_plus_neg_zero_first :: Property
prop_plus_neg_zero_first = 
  make_prop_binary_op_with_first (+) (+) signedNatToInteger (-50, 50) 
    (Neg Zero) 0

prop_plus_neg_zero_second :: Property
prop_plus_neg_zero_second = 
  make_prop_binary_op_with_second (+) (+) signedNatToInteger (-50, 50) 
    (Neg Zero) 0

prop_plus_pos_zero_first :: Property
prop_plus_pos_zero_first = 
  make_prop_binary_op_with_first (+) (+) signedNatToInteger (-50, 50) 
    (Pos Zero) 0

prop_plus_pos_zero_second :: Property
prop_plus_pos_zero_second = 
  make_prop_binary_op_with_second (+) (+) signedNatToInteger (-50, 50) 
    (Pos Zero) 0


prop_minus :: Property
prop_minus = 
  make_prop_binary_op (-) (-) signedNatToInteger (-50, 50)

prop_minus_neg_zero_first :: Property
prop_minus_neg_zero_first = 
  make_prop_binary_op_with_first (-) (-) signedNatToInteger (-50, 50) 
    (Neg Zero) 0

prop_minus_neg_zero_second :: Property
prop_minus_neg_zero_second = 
  make_prop_binary_op_with_second (-) (-) signedNatToInteger (-50, 50) 
    (Neg Zero) 0

prop_minus_pos_zero_first :: Property
prop_minus_pos_zero_first = 
  make_prop_binary_op_with_first (-) (-) signedNatToInteger (-50, 50) 
    (Pos Zero) 0

prop_minus_pos_zero_second :: Property
prop_minus_pos_zero_second = 
  make_prop_binary_op_with_second (-) (-) signedNatToInteger (-50, 50) 
    (Pos Zero) 0


prop_times :: Property
prop_times = 
  make_prop_binary_op (*) (*) signedNatToInteger (-50, 50)

prop_times_neg_zero_first :: Property
prop_times_neg_zero_first = 
  make_prop_binary_op_with_first (*) (*) signedNatToInteger (-50, 50) 
    (Neg Zero) 0

prop_times_neg_zero_second :: Property
prop_times_neg_zero_second = 
  make_prop_binary_op_with_second (*) (*) signedNatToInteger (-50, 50) 
    (Neg Zero) 0

prop_times_pos_zero_first :: Property
prop_times_pos_zero_first = 
  make_prop_binary_op_with_first (*) (*) signedNatToInteger (-50, 50) 
    (Pos Zero) 0

prop_times_pos_zero_second :: Property
prop_times_pos_zero_second = 
  make_prop_binary_op_with_second (*) (*) signedNatToInteger (-50, 50) 
    (Pos Zero) 0


prop_negate :: Property
prop_negate = 
  make_prop_unary_op negate negate signedNatToInteger (-50, 50)

prop_abs :: Property
prop_abs = 
  make_prop_unary_op abs abs signedNatToInteger (-50, 50)

prop_signum :: Property
prop_signum = 
  make_prop_unary_op signum signum signedNatToInteger (-50, 50)

----------------------------------------------------------------------
-- Run all the tests.
----------------------------------------------------------------------

{-
runTests :: IO Bool
runTests = $quickCheckAll
-}
-- HORRIBLE HACK FOR GHC 7.8.  See Quickcheck docs for more on this.
return []
runTests = $quickCheckAll

main :: IO ()
main = do
  success <- runTests
  if success
     then putStrLn "\nALL TESTS PASSED!\n"
     else putStrLn "\nERROR: SOME TESTS FAILED!\n"

