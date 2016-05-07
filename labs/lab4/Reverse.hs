module Main where
import System.Environment
import System.Exit

main :: IO ()
main = 
    do 
        progName <- getProgName -- Get program name
        args <- getArgs         -- Get program arguments 
        if length args /= 1 then
            -- Exit with failure if too many or too few command-line arguments.
            putStr "usage: " >>
            putStr progName >>
            putStrLn " filename" >>
            exitFailure else 
            -- Otherwise, print the contents of the file in reverse line order.
            do
                fContent <- readFile $ head args
                let fLines = reverse $ lines fContent 
                    fReversed = unlines fLines in
                    putStr fReversed >>
                    exitSuccess
