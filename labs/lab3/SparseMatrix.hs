{-# LANGUAGE NamedFieldPuns #-}
module SparseMatrix where

import qualified Data.Map as M
import qualified Data.Set as S 
import Data.Maybe

data SparseMatrix a =
  SM { bounds     :: (Integer, Integer),  -- number of rows, columns
       rowIndices :: S.Set Integer,       -- row indices with nonzeros
       colIndices :: S.Set Integer,       -- column indices with nonzeros
       vals       :: (M.Map (Integer, Integer) a) }  -- values
  deriving (Eq, Show)

-- ****************************************
-- ********** Part C **********************
-- ****************************************

-- C.1
-- The constructor for our SparseMatrix data type.
-- sparseMatrix <list of index/element pairs> <bounds> -> sparse matrix
sparseMatrix :: (Eq a, Num a) => 
  [((Integer, Integer), a)] -> (Integer, Integer) -> SparseMatrix a
-- Base case, where there are no more key-value pairs to insert.
sparseMatrix [] bounds@(_, _) = SM bounds S.empty S.empty M.empty
sparseMatrix (x:xs) bounds@(rows, cols) 
        | rows < 1  = error "row bound < 1" -- Error checking.
        | cols < 1  = error "col bound < 1"
        | otherwise = let                   -- Recursively create smaller matrix,
                                            -- then add to its values.
                         smallerMatrix = sparseMatrix xs bounds
                         rowSet = rowIndices smallerMatrix
                         colSet = colIndices smallerMatrix 
                         valMap = vals smallerMatrix 
                         currRow = fst $ fst x
                         currCol = snd $ fst x
                         currVal = snd x
                         in 
                         -- Check insert location and value. If value is 
                         -- non-zero and location is within bounds, update 
                         -- the smaller matrix. Else, just use the smaller 
                         -- matrix.
                         if currVal /= 0 && currRow <= rows && currCol <= cols 
                             && currRow >= 1 && currCol >= 1
                             then SM bounds (S.insert currRow rowSet) 
                                  (S.insert currCol colSet) 
                                  (M.insert (fst x) (snd x) valMap)
                             else smallerMatrix

-- C.2
-- This function adds two compatible sparse matrices. Matrices are deemed 
-- compatible if they have the same number of rows and columns.
-- If the matrices are compatible, we add by unioning the row and column
-- sets, and by doing a unionWith (with addition as the function) for the 
-- valMap.
addSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
addSM (SM { bounds = b1@(rows1, cols1), vals = valMap1 }) 
      (SM { bounds = (rows2, cols2), vals = valMap2 })
    | rows1 /= rows2 = error "adding matrices with unequal row numbers"
    | cols1 /= cols2 = error "adding matrices with unequal row numbers"
    | otherwise      = let newMap  = (M.unionWith (+) valMap1 valMap2)
                           -- Filter out additions that resulted in 0.
                           filtMap = M.filter (/= 0) newMap 
                           mapKeys = M.keys filtMap
                           in
                           SM b1 (S.fromList (map fst mapKeys)) (S.fromList (map snd mapKeys)) filtMap

-- C.3
-- Negates a sparse matrix.
-- Maps a function which multiplies by -1 over the values of the 
-- passed-in sparse matrix.
negateSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a
negateSM (SM { bounds, rowIndices, colIndices, vals }) = 
            SM bounds rowIndices colIndices (M.map (* (-1)) vals)

-- C.4 
-- Subtracts two compatible sparse matrices. Does this by negating the second 
-- argument, then adding it to the first.
subSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
subSM m1@(SM {}) m2@(SM {}) = addSM m1 (negateSM m2)

-- C.5
-- Multiplies two compatible sparse matrices A and B. First we take 
-- cartesian product of A's non-zero rows and B's non-zero columns to get 
-- all pairs of row/column indices we need to multiply. Then, for each 
-- such pair, we calculate the dot product of the corresponding row and column.
-- Finally, we zip the resulting values and locations into a list, and construct
-- a sparse matrix.
mulSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
mulSM (SM { bounds = (rows1, cols1), rowIndices = rowSet1, vals = valMap1 })
      (SM { bounds = (rows2, cols2), colIndices = colSet2, vals = valMap2 }) 
      | cols1 /= rows2 = error "matrix bounds are not compatible"
      | otherwise      = let 
                            pairs = [(x, y) | x <- S.toAscList rowSet1, y <- S.toAscList colSet2]
                            values = map (mulRC valMap1 valMap2) pairs
                            keyValues = zip pairs values
                            in
                            sparseMatrix keyValues (rows1, cols2) 
                            

-- Multiplies a row and column from our sparse matrix. The row and column 
-- are indicated by the last argument (the tuple). The row indexes into the 
-- matrix corresponding the the first map, and the column indexes into the 
-- matrix corresponding to the second map.
mulRC :: (Eq a, Num a) => (M.Map (Integer, Integer) a) -> (M.Map (Integer, Integer) a) -> 
                          (Integer, Integer) -> a
mulRC rowMap colMap (rowIndex, colIndex) = 
        -- The first filters are to get the entries corresponding to our 
        -- row and column indices; the second filters are to line up the 
        -- non-zero entries in the row and column we are multiplying.
        let filtRowMap  = M.filterWithKey (\(i, _) _ -> i == rowIndex) rowMap
            filtColMap  = M.filterWithKey (\(_, j) _ -> j == colIndex) colMap 
            filtRowMap2 = M.filterWithKey (\(_, j) _ -> M.member (j, colIndex) colMap) filtRowMap
            filtColMap2 = M.filterWithKey (\(i, _) _ -> M.member (rowIndex ,i) rowMap) filtColMap
            rowElems = M.elems filtRowMap2
            colElems = M.elems filtColMap2
            in 
            dotProduct rowElems colElems

dotProduct :: (Num a) => [a] -> [a] -> a
dotProduct = (sum .) . zipWith (*)

-- C.6
-- Retrieves a value from a sparse matrix given the row and column.
getSM :: Num a => SparseMatrix a -> (Integer, Integer) -> a
getSM (SM { bounds = (rows, cols), vals }) loc@(i, j)
    | i > rows || j > cols = error "index out of bounds error"
    | M.member loc vals    = fromJust $ M.lookup loc vals 
    | otherwise            = 0

-- Returns the number of rows in a sparse matrix.
rowsSM :: SparseMatrix a -> Integer 
rowsSM (SM { bounds = (rows, _) }) = rows

-- Returns the number of cols in a sparse matrix.
colsSM :: SparseMatrix a -> Integer 
colsSM (SM { bounds = (_, cols) }) = cols

-- C.7
(<|+|>) :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<|+|>) = addSM

(<|-|>) :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<|-|>) = subSM

(<|*|>) :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<|*|>) = mulSM

(<|!|>) :: Num a => SparseMatrix a -> (Integer, Integer) -> a
(<|!|>) = getSM

-- C.8
-- It doesn't make sense because you can't always take the sign of a matrix, 
-- since values in a matrix can have different signs. It's possible to take 
-- the sign of the determinant, but this isn't that consistent with other 
-- members of the Num class. It also doesn't make sense to convert an integer 
-- into a matrix, since, if anything, we would always just get a 1x1 matrix.
-- Thus, we wouldn't be able to define "signum" and "fromInteger", two needed 
-- functions for Num instances.
