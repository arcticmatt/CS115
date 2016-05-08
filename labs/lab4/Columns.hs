module Main where
import System.Environment
import System.Exit
import Data.Char

-- Main module to run the program.
main :: IO ()
main = 
    do 
        progName <- getProgName -- Get program name 
        args <- getArgs         -- Get program arguments 
        if length args < 2 || length (filter isPositiveInteger args) /= (length args) - 1 then 
            -- Exit with failure if too few command lines arguments, or if 
            -- column inputs are invalid (non-numbers/ints, or non-positive ints.
            putStrLn ("usage: " ++ progName ++ " n1 n2 ... filename (n1, n2, etc. are positive integers)") >>
            exitFailure else 
            let columns = map (\x -> read x :: Integer) $ take ((length args) - 1) args 
                fName = last args in
                if fName /= "-" then 
                    do -- Read from filename
                        fContent <- readFile fName 
                        printColumns fContent columns else
                    do -- Read from stdin
                        fContent <- getContents 
                        printColumns fContent columns

-- Given the file content as a String and a list of columns, print the 
-- column contents.
printColumns :: String -> [Integer] -> IO ()
printColumns fContent columns =
    let fLines = lines fContent 
        colLines = map (getColumns columns) fLines 
        colContents = unlines colLines in 
        putStr colContents >>
        exitSuccess

-- Gets the "columns" from a single string, where columns here correspond to 
-- space separated words, and returns them concatenated together in a space 
-- separated string.
getColumns :: [Integer] -> String -> String
getColumns xs line = iter xs (words line) (toInteger $ length $ words line) []
    where 
        -- Takes in the list of column numbers, the list of space separated words,
        -- the length of the list of words, and the result list (empty initially).
        -- Iterates through the list of column numbers; for each column number,
        -- if it is a valid index into the list of words, prepends the corresponding
        -- word to the result list. At the end, reverses the list, concatenates the 
        -- word, and returns the string.
        iter :: [Integer] -> [String] -> Integer -> [String] -> String 
        iter _ [] _ ans = unwords $ reverse ans
        iter [] _ _ ans = unwords $ reverse ans
        iter (x:xs) words hi ans = if x <= hi then 
                                       iter xs words hi ((words !! fromInteger (x - 1)) : ans) else
                                       iter xs words hi ans 

-- Checks if a string corresponds to a positive integers. Tests this by 
-- checking if all the characters of the string are digits, and also checking 
-- if at least one of the characters is a positive digit (so that the number
-- cannot be 0).
isPositiveInteger :: String -> Bool 
isPositiveInteger xs = all isDigit xs && any isPosDigit xs

-- Checks if a character corresponds to a positive digit.
isPosDigit :: Char -> Bool
isPosDigit x = elem x "123456789"
