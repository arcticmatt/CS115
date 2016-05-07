module Main where
import System.Environment
import System.Exit
import System.IO()
import Data.List

main :: IO ()
main = 
    do 
        progName <- getProgName
        args <- getArgs  -- Fails pattern matching on empty list
        if length args /= 1 then
            -- Exit with failure if too many or too few command-line arguments.
            putStr "usage: " >>
            putStr progName >>
            putStrLn " filename" >>
            exitFailure else 
            do
                fContent <- readFile $ head args
                let fLines = reverse $ lines fContent 
                    fReversed = intercalate "\n" fLines in
                    putStrLn fReversed >>
                    exitSuccess
