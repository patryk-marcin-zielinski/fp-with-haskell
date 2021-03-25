module MVar where

import Control.Concurrent.MVar
import Control.Concurrent

type Request a = MVar a
type Response b = MVar b

type Pipe a b = (Request a, Response b)

createPipe :: IO (Pipe a b) 
createPipe = do
    request <- newEmptyMVar 
    response <- newEmptyMVar 
    return (request, response)

sendRequest :: Pipe a b -> a -> IO b
sendRequest (request, response) a = do
    putMVar request a
    takeMVar response

worker :: Pipe a b -> (a -> b) -> IO () 
worker pipe@(request, response) f = do
    a <- takeMVar request 
    putMVar response (f a) 
    worker pipe f

isDivided x n = (x `mod` n) == 0

fizzBuzz x | isDivided x 15 = "FizzBuzz"
           | isDivided x 5 = "Buzz"
           | isDivided x 3 = "Fizz"
           | otherwise = show x

fizzBuzzProcessor :: Pipe Int String -> IO () 
fizzBuzzProcessor pipe = worker pipe fizzBuzz



generator :: Pipe Int String -> Int -> IO () 
generator pipe i = do
    result <- sendRequest pipe i
    putStrLn ("[" ++ show i ++ "]: " ++ result) 
    generator pipe (i + 1)


run = do
    pipe <- createPipe
    forkIO (fizzBuzzProcessor pipe) 
    forkIO (generator pipe 0)
