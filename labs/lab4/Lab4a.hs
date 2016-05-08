import Data.Char

-- ****************************************
-- ********** Part A **********************
-- ****************************************

-- A.1
-- myPutStrLn :: String -> IO ()
-- myPutStrLn "" = putChar '\n'
-- myPutStrLn (c:cs) = 
--   do putChar c
--      myPutStrLn cs
--
-- Seen above is a manual implementation of the putStrLn function. We will 
-- desugar this into a function that does not use the do notation.
myPutStrLn :: String -> IO ()
myPutStrLn "" = putChar '\n'
myPutStrLn (c:cs) = 
    putChar c >> myPutStrLn cs

-- A.2 
-- greet :: String -> IO ()
-- greet name = do putStrLn ("Hello, " ++ name ++ "!")
--
-- the "do" is not necessary, because putStrLn already has type String -> IO ().
-- Thus, we will simplify the code shown above as follows. 
greet :: String -> IO ()
greet name = putStrLn $ "Hello, " ++ name ++ "!"

-- A.3
-- Ask the user for his/her name, then print a greeting.
-- greet2 :: IO ()
-- greet2 = do
--   putStr "Enter your name: "
--   name <- getLine
--   putStr "Hello, "
--   putStr name
--   putStrLn "!"
--
-- We will first desugar this in the "simple" way (way 1).
greet2 :: IO ()
greet2 = 
    putStr "Enter your name: " >>
    getLine >>= \name ->
        putStr "Hello, " >>
        putStr name >> 
        putStrLn "!"

-- Now we will desugar this in the more complicated way (way 2). The complex
-- desugaring does not behave differently because the pattern matching never 
-- fails (the first pattern match binds to everything). If desired, we could 
-- change the first pattern match to bind to only non-empty strings.
greet2' :: IO ()
greet2' = 
    putStr "Enter your name: " >>
    getLine >>= 
        \name -> case name of 
            name -> 
                putStr "Hello, " >>
                putStr name >> 
                putStrLn "!"
            -- First pattern matches everything, so no failure necessary

-- A.4
-- Ask the user for his/her name, then print a greeting.
-- Capitalize the first letter of the name.
-- greet3 :: IO ()
-- greet3 = do
--   putStr "Enter your name: "
--   (n:ns) <- getLine
--   let name = toUpper n : ns
--   putStr "Hello, "
--   putStr name
--   putStrLn "!"
--
-- We will first desugar this in the "simple" way (way 1).
greet3 :: IO ()
greet3 =
    putStr "Enter your name: " >>
    getLine >>= \(n:ns) ->
        let name = toUpper n : ns in
            putStr "Hello, " >>
            putStr name >>
            putStrLn "!"

-- Now we will desugar this in the more complicated way (way 2).
-- Notice that complex desugaring DOES have an effect. In greet3', when the user
-- enters an empty string, a pattern match failure will be invoked and fail 
-- is invoked. This gives the user the error message "Pattern match failure 
-- in do expression." greet3 does not handle this error, and thus fails with 
-- the default error "Non-exhaustive patterns in lambda." We can see that 
-- practically, the same thing happens: both functions error out. However,
-- with the complex desugaring, we explicitly handle the error and return 
-- a more useful error message.
greet3' :: IO ()
greet3' = 
    putStr "Enter your name: " >>
    getLine >>=
        \line -> case line of 
            (n:ns) -> 
                let name = toUpper n : ns in 
                    putStr "Hello, " >> 
                    putStr name >>
                    putStrLn "!"
            _ ->
                fail "Pattern match failure in do expression"
