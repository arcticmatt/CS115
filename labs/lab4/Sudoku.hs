--
-- Sudoku.hs
--

--
-- This program reads a Sudoku problem from a file and
-- outputs the solution to stdout.
--

module Main where

import Control.Monad
import Data.Array.IO
import Data.Char
import Data.List as L
import Data.Set as S
import System.Environment
import System.Exit
import System.IO

usage :: IO ()
usage = hPutStrLn stderr $ "usage: sudoku filename"

type Sudoku = IOArray (Int, Int) Int


-- Read a file's contents into a Sudoku array.
readSudoku :: FilePath -> IO Sudoku
readSudoku f = do
  s <- readFile f
  let ls = lines s in
    if okLines ls
       then newListArray ((1, 1), (9, 9)) (L.map charToInt (concat ls))
       else error "readSudoku: invalid string input"
  where
    -- Check that the string input is a valid representation of a Sudoku board.
    okLines :: [String] -> Bool
    okLines ss =
      and [length ss == 9,
           all (\s -> length s == 9) ss,
           all okChar (concat ss)]

    okChar :: Char -> Bool
    okChar '.' = True
    okChar c | ord c >= ord '0' && ord c <= ord '9' = True
    okChar _ = False

    charToInt :: Char -> Int
    charToInt '.' = 0
    charToInt c   = ord c - ord '0'


-- Solve a Sudoku board.
-- Do this by iterating through the board, incrementing the unfilled numbers
-- by 1 until the right solution is found.
-- Return True if a solution is found, else false.
-- If a solution is found, the board contents will have mutated to the solution.
solveSudoku :: Sudoku -> IO Bool
solveSudoku s = iter s (1, 1)
   where
    -- Solve a Sudoku board starting from location (i, j).
    -- All "previous" locations are assumed to have been filled.
    -- If the board is solveable, return True; if not, return False.
    -- In the latter case the board will not have changed.
     iter :: Sudoku -> (Int, Int) -> IO Bool
     iter sBoard pos@(row, col) = 
       do val <- readArray sBoard pos 
          if val == 0 -- If we're at an empty space, try to fill it.
            then do values <- getOKValues sBoard pos
                    iter' sBoard pos values
            else if col == 9 -- Otherwise, advance to the next empty space.
                   then let nextRow = row + 1
                            nextCol = 1 
                            in if nextRow > 9
                                 then return True -- End of board, return true.
                                 else iter sBoard (nextRow, nextCol)
                   else let nextRow = row 
                            nextCol = col + 1
                            in iter sBoard (nextRow, nextCol)

    -- Try to solve the board using all possible currently-valid
    -- values at a particular location.
    -- If the board is unsolveable, reset the location to a zero
    -- (unmake the move) and return False.
     iter' :: Sudoku -> (Int, Int) -> [Int] -> IO Bool
     iter' _ _ [] = return False -- If no valid values, return false.
     iter' sBoard pos@(row, col) (x:xs) = 
      do
        writeArray sBoard pos x -- Write a possible value to the board.
        if col == 9 -- If we're at the end of a row, advance to the next row.
          then let nextRow = row + 1
                   nextCol = 1
                   in if nextRow > 9
                        then return True -- If we finish the board, return true.
                        -- Try to solve with current move.
                        else do solveable <- iter sBoard (nextRow, nextCol)
                                if not solveable 
                                  then do writeArray sBoard pos 0 -- Unmake the move.
                                          iter' sBoard pos xs -- Redo current move.
                                  else return solveable
          else let nextRow = row -- Else, advance to the next column.
                   nextCol = col + 1
                   -- Try to solve with current move.
                   in do solveable <- iter sBoard (nextRow, nextCol)
                         if not solveable 
                           then do writeArray sBoard pos 0 -- Unmake the move.
                                   iter' sBoard pos xs -- Redo current move.
                           else return solveable
                   
    -- Get a list of indices that could be in a particular location on the 
    -- board (no conflicts in row, column, or box).
     getOKValues :: Sudoku -> (Int, Int) -> IO [Int]
     getOKValues sBoard pos@(row, col) =
      do rowVals <- getRow sBoard row 
         colVals <- getCol sBoard col 
         boxVals <- getBox sBoard pos
         let usedVals = S.fromList (rowVals ++ colVals ++ boxVals)
             allVals = fromList [1, 2, 3, 4, 5, 6, 7, 8, 9] 
           -- To get possible values, subtract used values from set of valid values.
           in return $ S.toList $ S.difference allVals usedVals

    -- Return the ith row in a Sudoku board as a list of Ints.
     getRow :: Sudoku -> Int -> IO [Int]
     getRow sBoard row 
      | row < 1 || row > 9 = error "row index out of bounds"
      |otherwise           = iter sBoard row 1 []
        where
          -- Iterates through a row of the board.
          iter :: Sudoku -> Int -> Int -> [Int] -> IO [Int] 
          iter sBoard i j xs =
            if (j > 9)
              then return $ reverse xs
              else (do c <- readArray sBoard (i, j)
                       iter sBoard i (j + 1) (c : xs))

    -- Return the ith column in a Sudoku board as a list of Ints.
     getCol :: Sudoku -> Int -> IO [Int]
     getCol sBoard col
      | col < 1 || col > 9 = error "col index out of bounds"
      | otherwise          = iter sBoard 1 col []
        where 
          -- Iterates through a column of the board.
          iter :: Sudoku -> Int -> Int -> [Int] -> IO [Int]
          iter sBoard i j xs =
            if (i > 9)
              then return $ reverse xs
              else (do c <- readArray sBoard (i, j)
                       iter sBoard (i + 1) j (c : xs))

    -- Return the box covering location (i, j) as a list of Ints.
     getBox :: Sudoku -> (Int, Int) -> IO [Int]
     getBox sBoard pos = 
      let (startRow, startCol) = getBoxCorner pos
          endPos               = (startRow + 2, startCol + 2)
          in iter sBoard (startRow, startCol) endPos []
          where
            -- Iterates through a 3 x 3 box of the board.
            iter :: Sudoku -> (Int, Int) -> (Int, Int) -> [Int] -> IO [Int]
            iter sBoard (row, col) endPos@(endRow, endCol) xs =
              if (row > endRow)
                then return $ reverse xs
                else (do c <- readArray s (row, col)
                         if col == endCol 
                          then iter sBoard ((row + 1), (endCol - 2)) endPos (c : xs)
                          else iter sBoard (row, (col + 1)) endPos (c : xs))

            -- Returns the top-left corner of the box in which the passed-in
            -- position falls. E.g. if the position is (2, 2), (1, 1) will be
            -- returned, because that is the top-left corner of the top-left
            -- box.
            getBoxCorner :: (Int, Int) -> (Int, Int)
            getBoxCorner (row, col) = 
              let row' = row - 1
                  col' = col - 1
                  rowBox = row' `quot` 3
                  colBox = col' `quot` 3
                  in (rowBox * 3 + 1, colBox * 3 + 1)

-- Print a Sudoku board to stdout.
printSudoku :: Sudoku -> IO ()
printSudoku s = iter s 1 1
  where
    iter :: Sudoku -> Int -> Int -> IO ()
    iter s i j = 
      unless (i > 9)
        (do c <- readArray s (i, j)
            putChar $ intToChar c
            if j == 9 
               then putChar '\n' >> iter s (i + 1) 1
               else iter s i (j + 1))

    intToChar :: Int -> Char
    intToChar 0 = '.'
    intToChar n | n >= 1 && n <= 9 = intToDigit n
    intToChar m = error $ "printSudoku: invalid integer in array: " ++ show m


main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
     then usage >> exitFailure
     else
       do sudoku <- readSudoku (head args) -- read board contents into array
          solved <- solveSudoku sudoku
          if solved
             then printSudoku sudoku >> exitSuccess
             else putStrLn "No solution exists." >> exitFailure
