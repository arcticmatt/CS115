module Main where
import System.Environment
import System.Exit
import Data.Char

-- Main module to run the program.
-- TODO: avoid code repeats?
main :: IO ()
main = 
    do 
        progName <- getProgName -- Get program name 
        args <- getArgs         -- Get program arguments 
        if length args < 2 || length (filter isPositiveInteger args) /= (length args) - 1 then 
            -- Exit with failure if too few command lines arguments, or if 
            -- column inputs are invalid (non-numbers/ints, or non-positive ints.
            putStr "usage: " >>
            putStr progName >>
            putStrLn " n1 n2 ... filename (n1, n2, etc. are positive integers)" >>
            exitFailure else 
            let columns = map (\x -> read x :: Integer) $ take ((length args) - 1) args 
                fName = last args in
                if fName /= "-" then 
                    do -- Read from filename
                        fContent <- readFile fName 
                        let fLines = lines fContent 
                            colLines = map (getColumns columns) fLines 
                            colContents = unlines colLines in 
                            putStr colContents >>
                            exitSuccess else  
                    do -- Read from stdin
                        fContent <- getContents 
                        let fLines = lines fContent 
                            colLines = map (getColumns columns) fLines 
                            colContents = unlines colLines in 
                            putStr colContents >>
                            exitSuccess

-- Gets the "columns" from a single string, where columns here correspond to 
-- space separated words, and returns them concatenated together in a space 
-- separated string.
getColumns :: [Integer] -> String -> String
getColumns xs line = iter xs (zip [1..toInteger $ length $ words line] (words line)) []
    where 
        -- Iterates through the list of column numbers and a list of 
        -- (index, word) tuples. When column number == index, adds the corresponding
        -- word to the result list. At the end, reverses the result list and
        -- concatenates it.
        iter :: [Integer] -> [(Integer, String)] -> [String] -> String 
        iter _ [] ans = unwords $ reverse ans
        iter [] _ ans = unwords $ reverse ans
        iter cols@(x:xs) (y:ys) ans = if x == fst y then
                                         iter xs ys (snd y : ans) else 
                                         iter cols ys ans 

-- Checks if a string corresponds to a positive integers. Tests this by 
-- checking if all the characters of the string are digits, and also checking 
-- if at least one of the characters is a positive digit (so that the number
-- cannot be 0).
isPositiveInteger :: String -> Bool 
isPositiveInteger xs = all isDigit xs && any isPosDigit xs

-- Checks if a character corresponds to a positive digit.
isPosDigit :: Char -> Bool
isPosDigit x = elem x "123456789"
