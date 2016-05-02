--
-- SparseMatrixTests.hs
--     Tests of sparse matrices.
--

{-# LANGUAGE TemplateHaskell #-}

module SparseMatrixTests where

import qualified Data.Map as M
import qualified Data.Set as S 

import Test.HUnit
import Test.QuickCheck
-- NOT NEEDED FOR GHC 7.8:
-- import Test.QuickCheck.All

import SparseMatrix

----------------------------------------------------------------------
-- Helper functions.
----------------------------------------------------------------------

-- Check that bounds are valid.
okBounds :: (Integer, Integer) -> Bool
okBounds (x, y) = x >= 1 && y >= 1

----------------------------------------------------------------------
-- Special sparse matrices.
----------------------------------------------------------------------

-- A zero matrix with particular bounds.
zeroSM :: (Integer, Integer) -> SparseMatrix Integer
zeroSM b | okBounds b = SM b S.empty S.empty M.empty
zeroSM _ = error "zeroSM: invalid bounds"

-- Square identity matrix.
identitySM :: Integer -> SparseMatrix Integer
identitySM size | size > 1 =
  SM (size, size) allIndices allIndices identityMap
  where
    allIndices :: S.Set Integer
    allIndices = S.fromList [1..size]

    identityMap :: M.Map (Integer, Integer) Integer
    identityMap = M.fromList [((i, i), 1) | i <- [1..size]]

identitySM _ = error "identitySM: invalid size"

----------------------------------------------------------------------
-- Randomly generated sparse matrices of Integers.
----------------------------------------------------------------------

-- Generate a list of unique pairs of integers in a particular range,
-- usable as indices into a sparse matrix.
-- TODO: There are probably way more elegant ways to compute this
-- (explicit recursion alert!).
genIndices :: Int -> (Integer, Integer) -> Gen [(Integer, Integer)]
genIndices n (nrows, ncols) = iter [] n nrows ncols
  where
    iter :: [(Integer, Integer)] -> Int -> Integer -> Integer ->
      Gen [(Integer, Integer)]
    iter vals 0 _ _ = return vals
    iter vals n nrows ncols = do
      i <- choose (1, nrows)
      j <- choose (1, ncols)
      if (i, j) `elem` vals 
         then iter vals n nrows ncols
         else iter ((i, j) : vals) (n - 1) nrows ncols

-- Generate a sparse matrix with 
-- a) a given number of nonzeros
-- b) given bounds for the nonzeros (must be > 0)
-- c) given numbers of rows and columns
randomSM :: Int -> (Integer, Integer) -> (Integer, Integer) 
  -> Gen (SparseMatrix Integer)
randomSM nonzeros _ _ | nonzeros < 0 = 
  error "randomSM: invalid number of nonzeros"
randomSM _ (imin, imax) _ | imin > imax =
  error "randomSM: invalid bounds for values"
randomSM _ _ (nrows, ncols) | nrows < 1 || ncols < 1 =
  error "randomSM: too few rows or columns"
randomSM nonzeros _ (nrows, ncols) | toInteger nonzeros > nrows * ncols =
  error "randomSM: too many nonzeros"
randomSM nonzeros valbounds dims = do
  -- Generate the random array contents.
  nums <- vectorOf nonzeros $ choose valbounds
  -- Generate random array indices.
  -- These must fall within the given bounds and there must be no duplicates.
  indices <- genIndices nonzeros dims
  let ivals = zip indices nums
  return $ sparseMatrix ivals dims


----------------------------------------------------------------------
-- Tests on sparse matrices.
-- These can be used to construct properties.
----------------------------------------------------------------------

-- Test that the sparse matrix map contains no zeros.
test_SM_no_zeros :: SparseMatrix Integer -> Bool
test_SM_no_zeros = all (/= 0) . M.elems . vals

-- Test that the indices in the sparse matrix map are all in bounds.
test_SM_inBounds :: SparseMatrix Integer -> Bool
test_SM_inBounds m = 
  and [inBounds k (bounds m) | k <- M.keys (vals m)]
  where
    inBounds :: (Integer, Integer) -> (Integer, Integer) -> Bool
    inBounds (x, y) (rows, cols) = x >= 1 && y >= 1 && x <= rows && y <= cols

-- Test that the sparse matrix row indices are correct i.e. they contain
-- all and only the nonzero row indices.
test_SM_rowIndices :: SparseMatrix Integer -> Bool
test_SM_rowIndices m =
  rowIndices m == getNonzeroRows m
  where
    -- Get the nonzero rows of the matrix.
    getNonzeroRows :: SparseMatrix Integer -> S.Set Integer
    getNonzeroRows = S.fromList . map fst . M.keys . M.filter (/= 0) . vals


-- Test that the sparse matrix column indices are correct i.e. they contain
-- all and only the nonzero column indices.
test_SM_columnIndices :: SparseMatrix Integer -> Bool
test_SM_columnIndices m =
  colIndices m == getNonzeroColumns m
  where
    -- Get the nonzero columns of the matrix.
    getNonzeroColumns :: SparseMatrix Integer -> S.Set Integer
    getNonzeroColumns = S.fromList . map snd . M.keys . M.filter (/= 0) . vals

-- Test that all values in the matrix satisfy some predicate.
test_SM_vals :: (Integer -> Bool) -> SparseMatrix Integer -> Bool
test_SM_vals p = all p . M.elems . vals


----------------------------------------------------------------------
-- Properties for sparse matrices.
----------------------------------------------------------------------

--
-- 1) Testing the correct generation of sparse matrices.
--

-- A matrix comprised of a single value at selected locations plus
-- zeros everywhere else must only store that value.
prop_SM_no_zeros :: Property
prop_SM_no_zeros = 
  forAll ones check
  where
    -- Sparse matrix containing only 1s.
    ones :: Gen (SparseMatrix Integer)
    ones = randomSM 10 (1, 1) (10, 10)

    -- Check that there are exactly 10 1s and no zeros in the elements
    -- stored in the matrix.
    check :: SparseMatrix Integer -> Bool
    check m =
      let es = M.elems (vals m) in
      if 0 `elem` es
         then False
         else all (== 1) es

-- A matrix created from a list containing zeros doesn't store
-- the zeros in the sparse matrix.
prop_SM_no_zeros_from_list :: Property
prop_SM_no_zeros_from_list =
  forAll contents check
  where
    -- contents generates a list of 100 integers, where roughly
    -- half are zeros and the other half are nonzeros.
    contents :: Gen [Integer]
    contents = 
      vectorOf 100 (choose (False, True)) >>=
        sequence . map (\b -> if b then return 0 else choose (1, 10))

    check :: [Integer] -> Bool
    check vs =
      let 
        bounds = [(i, j) | i <- [1..10], j <- [1..10]]
        m      = sparseMatrix (zip bounds vs) (10, 10)
        es     = M.elems (vals m) 
      in
         not (0 `elem` es)

-- A sparse matrix must have all indices contained within the stated bounds.
prop_SM_inbounds :: Property
prop_SM_inbounds =
  forAll mat test_SM_inBounds
  where
    -- Sparse matrix containing positive values.
    mat :: Gen (SparseMatrix Integer)
    mat = randomSM 15 (1, 100) (10, 20)

-- A sparse matrix must have all the nonzero rows in rowIndices and
-- only those rows.
prop_SM_rowIndices :: Property
prop_SM_rowIndices =
  forAll mat test_SM_rowIndices
  where
    -- Sparse matrix containing positive values.
    mat :: Gen (SparseMatrix Integer)
    mat = randomSM 15 (1, 100) (10, 20)

-- A sparse matrix must have all the nonzero columns in columnIndices and
-- only those columns.
prop_SM_columnIndices :: Property
prop_SM_columnIndices =
  forAll mat test_SM_columnIndices
  where
    -- Sparse matrix containing positive values.
    mat :: Gen (SparseMatrix Integer)
    mat = randomSM 15 (1, 100) (10, 20)

--
-- 2) Testing addition, subtraction, negation.
--

-- Adding together two random matrices doesn't generate zeros.
prop_SM_addNozeros :: Property
prop_SM_addNozeros =
  forAll mats check
  where
    mats :: Gen (SparseMatrix Integer, SparseMatrix Integer)
    mats = do
      mat1 <- randomSM 15 (1, 100) (10, 20)
      mat2 <- randomSM 15 (1, 100) (10, 20)
      return (mat1, mat2)

    check :: (SparseMatrix Integer, SparseMatrix Integer) -> Bool
    check (m1, m2) = test_SM_no_zeros $ addSM m1 m2

-- Negating a matrix generates no zeros.
prop_SM_negateNozeros :: Property
prop_SM_negateNozeros =
  forAll mat check
  where
    -- Sparse matrix containing positive values.
    mat :: Gen (SparseMatrix Integer)
    mat = randomSM 15 (1, 100) (10, 20)

    check :: SparseMatrix Integer -> Bool
    check m = test_SM_no_zeros (negateSM m)

-- Negating a matrix with positive values gives a matrix with negative values.
prop_SM_negatePositiveToNegative :: Property
prop_SM_negatePositiveToNegative =
  forAll mat check
  where
    -- Sparse matrix containing positive values.
    mat :: Gen (SparseMatrix Integer)
    mat = randomSM 15 (1, 100) (10, 20)

    check :: SparseMatrix Integer -> Bool
    check m = test_SM_vals (< 0) (negateSM m)

-- Adding a matrix to its negation generates the zero matrix.
prop_SM_negateToZero :: Property
prop_SM_negateToZero =
  forAll mat check
  where
    -- Sparse matrix containing positive values.
    mat :: Gen (SparseMatrix Integer)
    mat = randomSM 15 (1, 100) (10, 20)

    check :: SparseMatrix Integer -> Bool
    check m = addSM m (negateSM m) == (zeroSM (bounds m))

-- Subtracting a matrix from itself generates the zero matrix.
prop_SM_subtractSelf :: Property
prop_SM_subtractSelf =
  forAll mat check
  where
    -- Sparse matrix containing positive values.
    mat :: Gen (SparseMatrix Integer)
    mat = randomSM 15 (1, 100) (10, 20)

    check :: SparseMatrix Integer -> Bool
    check m = subSM m m == (zeroSM (bounds m))

-- Adding a matrix to itself gives a matrix with values twice the size,
-- and the same bounds, rowIndices and colIndices.
prop_SM_double :: Property
prop_SM_double =
  forAll mat check
  where
    -- Sparse matrix containing positive values.
    mat :: Gen (SparseMatrix Integer)
    mat = randomSM 15 (1, 100) (10, 20)

    check :: SparseMatrix Integer -> Bool
    check m = 
      let m2 = addSM m m in
      and [bounds m == bounds m2,
           rowIndices m == rowIndices m2,
           colIndices m == colIndices m2,
           vals m2 == M.map (2*) (vals m)]

-- Adding a matrix and subtracting the same matrix gives the original matrix.
prop_SM_addSubtract :: Property
prop_SM_addSubtract =
  forAll mats check
  where
    mats :: Gen (SparseMatrix Integer, SparseMatrix Integer)
    mats = do
      mat1 <- randomSM 15 (1, 100) (10, 20)
      mat2 <- randomSM 15 (1, 100) (10, 20)
      return (mat1, mat2)

    check :: (SparseMatrix Integer, SparseMatrix Integer) -> Bool
    check (m1, m2) =
      let m3 = addSM m1 m2
          m4 = subSM m3 m2
      in m1 == m4

--
-- 3) Testing multiplication.
--

-- Multiplying a square matrix by the zero matrix gives the zero matrix.
prop_SM_mulZero :: Property
prop_SM_mulZero =
  forAll mat check
  where
    -- Square sparse matrix containing positive values.
    mat :: Gen (SparseMatrix Integer)
    mat = randomSM 10 (1, 100) (10, 10)

    check :: SparseMatrix Integer -> Bool
    check m = mulSM m (zeroSM (bounds m)) == zeroSM (bounds m)

-- Multiplying a square matrix by the identity matrix gives the original matrix.
prop_SM_mulIdentity :: Property
prop_SM_mulIdentity =
  forAll mat check
  where
    -- Square sparse matrix containing positive values.
    mat :: Gen (SparseMatrix Integer)
    mat = randomSM 10 (1, 100) (10, 10)

    check :: SparseMatrix Integer -> Bool
    check m = 
      let size = fst (bounds m) in
        mulSM m (identitySM size) == m

-- Multiplying a square matrix by twice the identity matrix gives twice
-- the square matrix.
prop_SM_mulDouble :: Property
prop_SM_mulDouble =
  forAll mat check
  where
    -- Square sparse matrix containing positive values.
    mat :: Gen (SparseMatrix Integer)
    mat = randomSM 10 (1, 100) (10, 10)

    check :: SparseMatrix Integer -> Bool
    check m = 
      let size = fst (bounds m) 
          twiceIdentity = addSM (identitySM size) (identitySM size)
      in
        mulSM m twiceIdentity == addSM m m

-- Multiplying an NxM matrix with an MxP matrix, both with positive elements,
-- gives an NxP matrix with positive elements.
prop_SM_mulMNP :: Property
prop_SM_mulMNP =
  forAll mats check
  where
    -- Sparse matrices containing positive values.
    -- They are compatible for multiplication.
    mats :: Gen (SparseMatrix Integer, SparseMatrix Integer)
    mats = do
      mat1 <- randomSM 15 (1, 100) (10, 20)
      mat2 <- randomSM 15 (1, 100) (20, 15)
      return (mat1, mat2)

    check :: (SparseMatrix Integer, SparseMatrix Integer) -> Bool
    check (m1, m2) = 
      let m3 = mulSM m1 m2 in
        and [test_SM_no_zeros m3,
             test_SM_vals (> 0) m3,
             bounds m3 == (fst (bounds m1), snd (bounds m2))]

--
-- 4) Testing accessors.
--

-- Getting a diagonal value from the identity matrix will be 1.
-- Getting a non-diagonal value will be zero.
prop_SM_getIdentity :: Property
prop_SM_getIdentity =
  forAll ints check
  where
    identity1000 :: SparseMatrix Integer
    identity1000 = identitySM 1000

    -- Pairs of integers between 1 and 1000.
    ints :: Gen (Integer, Integer)
    ints = do
      i1 <- choose (1, 1000)
      i2 <- choose (1, 1000)
      return (i1, i2)

    check :: (Integer, Integer) -> Bool
    check (i1, i2) = 
      and [getSM identity1000 (i1, i1) == 1,
           getSM identity1000 (i2, i2) == 1,
           if i1 /= i2 
             then getSM identity1000 (i1, i2) == 0 
                  && getSM identity1000 (i2, i1) == 0
             else True]

----------------------------------------------------------------------
-- HUnit tests.
----------------------------------------------------------------------

-- Sample 4x4 matrices.

sm_sample1 :: SparseMatrix Integer
sm_sample1 =
  let 
    vs  = [1, 0, 2, 0, 0, 0, 0, 0, 3, 0, 4, 0, 0, 0, 0, 0]
    is  = [(i, j) | i <- [1..4], j <- [1..4]]
    ivs = zip is vs
  in 
    sparseMatrix ivs (4, 4)

sm_sample2 :: SparseMatrix Integer
sm_sample2 =
  let 
    vs  = [5, 0, 6, 0, 0, 0, 0, 0, 7, 0, 8, 0, 0, 0, 0, 0]
    is  = [(i, j) | i <- [1..4], j <- [1..4]]
    ivs = zip is vs
  in 
    sparseMatrix ivs (4, 4)

sm_sample3 :: SparseMatrix Integer
sm_sample3 =
  let 
    vs  = [6, 0, 4, 0, 0, 0, 0, 0, -3, 0, -2, 0, 0, 0, 0, 0]
    is  = [(i, j) | i <- [1..4], j <- [1..4]]
    ivs = zip is vs
  in 
    sparseMatrix ivs (4, 4)

sm_sample1plus2 :: SparseMatrix Integer
sm_sample1plus2 =
  let 
    vs  = [6, 0, 8, 0, 0, 0, 0, 0, 10, 0, 12, 0, 0, 0, 0, 0]
    is  = [(i, j) | i <- [1..4], j <- [1..4]]
    ivs = zip is vs
  in 
    sparseMatrix ivs (4, 4)

sm_sample1plus3 :: SparseMatrix Integer
sm_sample1plus3 =
  let 
    vs  = [7, 0, 6, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0]
    is  = [(i, j) | i <- [1..4], j <- [1..4]]
    ivs = zip is vs
  in 
    sparseMatrix ivs (4, 4)

sm_sample1times2 :: SparseMatrix Integer
sm_sample1times2 =
  let 
    vs  = [19, 0, 22, 0, 0, 0, 0, 0, 43, 0, 50, 0, 0, 0, 0, 0]
    is  = [(i, j) | i <- [1..4], j <- [1..4]]
    ivs = zip is vs
  in 
    sparseMatrix ivs (4, 4)

sm_sample1times3 :: SparseMatrix Integer
sm_sample1times3 =
  let 
    vs  = [0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 4, 0, 0, 0, 0, 0]
    is  = [(i, j) | i <- [1..4], j <- [1..4]]
    ivs = zip is vs
  in 
    sparseMatrix ivs (4, 4)

-- Unit tests.

test1 :: Test
test1 = TestCase $
  assertEqual "matrix addition 1"
    sm_sample1plus2 (addSM sm_sample1 sm_sample2)

test2 :: Test
test2 = TestCase $
  assertEqual "matrix addition 2"
    sm_sample1plus3 (addSM sm_sample1 sm_sample3)

test3 :: Test
test3 = TestCase $
  assertEqual "matrix addition 3"
    sm_sample1plus2 (sm_sample1 <|+|> sm_sample2)

test4 :: Test
test4 = TestCase $
  assertEqual "matrix addition 4"
    sm_sample1plus3 (sm_sample1 <|+|> sm_sample3)

test5 :: Test
test5 = TestCase $
  assertEqual "matrix multiplication 1"
    sm_sample1times2 (mulSM sm_sample1 sm_sample2)

test6 :: Test
test6 = TestCase $
  assertEqual "matrix multiplication 2"
    sm_sample1times3 (mulSM sm_sample1 sm_sample3)

test7 :: Test
test7 = TestCase $
  assertEqual "matrix multiplication 3"
    sm_sample1times2 (sm_sample1 <|*|> sm_sample2)

test8 :: Test
test8 = TestCase $
  assertEqual "matrix multiplication 4"
    sm_sample1times3 (sm_sample1 <|*|> sm_sample3)

tests :: Test
tests = TestList [TestLabel "test1" test1,
                  TestLabel "test2" test2,
                  TestLabel "test3" test3,
                  TestLabel "test4" test4,
                  TestLabel "test5" test5,
                  TestLabel "test6" test6,
                  TestLabel "test7" test7,
                  TestLabel "test8" test8]

----------------------------------------------------------------------
-- Run all the tests.
----------------------------------------------------------------------

-- Run all the quickcheck tests.
-- HORRIBLE HACK FOR GHC 7.8:
{-
runTests :: IO Bool
runTests = $quickCheckAll
-}
return []
runTests = $quickCheckAll

-- Run all the tests.
main :: IO ()
main = do
  putStrLn "Running QuickCheck tests...\n"
  success <- runTests
  if success
     then putStrLn "\nALL TESTS PASSED!\n"
     else putStrLn "\nERROR: SOME TESTS FAILED!\n"
  putStrLn "Running HUnit tests...\n"
  counts <- runTestTT tests
  print counts
  putStrLn ""

