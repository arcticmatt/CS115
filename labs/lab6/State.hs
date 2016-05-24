-- Need to import these modules for this code to work:
import Control.Monad
import Control.Monad.State
import Data.IORef

-- While loop in the IO monad.
whileIO :: IO Bool -> IO () -> IO () 
whileIO test block = 
  do b <- test 
     when b (block >> whileIO test block)

-- While loop in the state monad.
whileState :: (s -> Bool) -> State s () -> State s ()
whileState test body = 
  do s0 <- get
     when (test s0)
          (do modify (execState body)
              whileState test body)

-- ****************************************
-- ********** Part A **********************
-- ****************************************

-- A.1
-- Here, we will write a function called factIO which computes factorials and 
-- works in the IO monad.
factIO :: Integer -> IO Integer 
factIO x | x < 0     = error "factIO: invalid input, requires non-negative integer"
         | otherwise = factIOHelper x 1 where
  factIOHelper :: Integer -> Integer -> IO Integer
  factIOHelper x ans =
    do n <- newIORef x
       m <- newIORef ans
       whileIO
         (liftM2 (/=) (return 0) (readIORef n)) -- lift 0 to compare to monad value
         (do n' <- readIORef n
             m' <- readIORef m
             writeIORef m (m' * n')
             writeIORef n (n' - 1))
       readIORef m

-- A.2
-- Here, we will write a function called factState that computes factorials
-- and uses a state monad.
factState :: Integer -> Integer 
factState x | x < 0     = error "factState: invalid input, requires non-negative integer"
            | otherwise = evalState factStateHelper (x, 1) where 
  factStateHelper :: State (Integer, Integer) Integer 
  factStateHelper = 
    do whileState (\(n, _) -> n /= 0)
         (do (n, m) <- get 
             put (n - 1, n * m))
       (_, m) <- get 
       return m

-- A.3
-- Here, we will write a function called fibIO that computes fibonacci numbers 
-- and uses the IO monad. We will use the typical definition of fibonacci
-- numbers, e.g. fib(0) == 0, fib(1) == 1, etc.
fibIO :: Integer -> IO Integer 
fibIO x | x < 0     = error "fibIO: invalid input, requires non-negative integer"
        | otherwise = fibIOHelper x 0 1 where 
  fibIOHelper :: Integer -> Integer -> Integer -> IO Integer 
  fibIOHelper x prev1 prev2 =
    do n  <- newIORef x 
       p1 <- newIORef prev1
       p2 <- newIORef prev2
       whileIO
         (liftM2 (/=) (return 0) (readIORef n))
         (do n'  <- readIORef n
             p1' <- readIORef p1
             p2' <- readIORef p2 
             writeIORef n  (n' - 1)
             writeIORef p1 (p2')
             writeIORef p2 (p1' + p2'))
       readIORef p1

-- A.4
-- Here, we will write a function called fibState that computes fibonacci 
-- numbers and uses a state monad. 
fibState :: Integer -> Integer 
fibState x | x < 0     = error "fibState: invalid input, requires non-negative integer"
           | otherwise = evalState fibStateHelper (x, 0, 1) where 
  fibStateHelper :: State (Integer, Integer, Integer) Integer 
  fibStateHelper =
    do whileState (\(n, _, _) -> n /= 0)
         (do (n, p1, p2) <- get 
             put (n - 1, p2, p1 + p2))
       (_, p1, _) <- get 
       return p1

-- ****************************************
-- ********** Part B **********************
-- ****************************************

-- B.1
{-
 - DERIVING >>=
 - f' :: (a, r) -> b 
 - g' :: (b, r) -> c
 - h' :: (a, r) -> c
 - h' (x, rd) = 
 -   let y = f' (x, rd)
 -       z = g' (y, rd)
 -   in (z, rd)
 -
 - f :: a -> Reader r b
 - f x = Reader (\rd -> f' (x, rd))
 - g :: b -> Reader r c
 - g x = Reader (\rd -> g' (x, rd))
 - h :: a -> Reader r c
 - h x = Reader (\rd -> h' (x, rd))
 -
 -
 - h = f >=> g 
 - h x = f x >>= g
 - f x >>= g = h x 
 -
 - Substitute definition of "h x"
 - 
 - f x >>= g = Reader (\rd -> h' (x, rd))
 -          =  Reader (\rd -> 
 -               let y = f' (x, rd)
 -                   z = g' (y, rd)
 -               in (z, rd))
 -          =  Reader (\rd -> 
 -               let y = f' (x, rd)
 -               in g' (y, rd))
 -
 - Recall:
 - f x = Reader (\rd -> f' (x, rd))
 -
 - f x >>= g = Reader (\rd -> 
 -               let (Reader ff) = f x 
 -                   -- ff = \rd -> f' (x rd)
 -                   y = ff rd
 -               in g' (y, rd))
 -
 - Recall:
 - g x = Reader (\rd -> g' (x, rd))
 -
 - f x >>= g = Reader (\rd -> 
 -               let (Reader ff) = f x 
 -                   y = ff rd
 -                   (Reader gg) = g y
 -                   -- gg = \rd -> g' (y, rd)
 -               in gg rd)
 -
 - Substitute mx for f x to get: 
 -
 - mx >>= g = Reader (\rd -> 
 -              let (Reader ff) = mx
 -                  y = ff rd
 -                  (Reader gg) = g y
 -                  -- gg = \rd -> g' (y, rd)
 -              in gg rd)
 - 
 - This is equivalent (substituting letters) to the definition shown in
 - the assignment,
 -
 - mx >>= f = Reader (\r -> 
 -              let (Reader g) = mx
 -                  x = g r
 -                  (Reader h) = f x
 -              in h r)
 - 
 - which is also equivalent to 
 -
 - mx >>= f = Reader (\r ->
 -              let x = runReader mx r in
 -                runReader (f x) r)
 -
 - 
 - DERIVING return
 - Recall: the return method for a particular monad is the monadic version of 
 - the identity function. 
 -
 - The non-monadic reader identity functions would be as follows.
 - id_reader :: (a, r) -> a
 - id_reader (x, rd) = x
 - id_reader' :: a -> r -> a
 - id_reader' x rd = x 
 - id_reader' x = \rd -> x
 -
 - Written as a function in the (Reader r) monad, this becomes 
 - id_reader_monad :: a -> Reader r a 
 - id_reader_monad x = Reader (\rd -> x)
 -
 - This is the identity function in the (Reader r) monad. Therfore, it is also
 - the return method.
 - return :: a -> Return r a 
 - return x = Reader (\rd -> x)
 - 
 - Note: return takes a value and outputs a Reader that takes a read-only
 - value and returns the original value.
 -}

