module Factorial where

import Control.Monad.Reader
import Control.Monad.State as S
import Control.Monad.Writer 
import Control.Monad.Trans.State as ST

type StateIO state returnval = StateT state IO returnval

factorialProducts :: Integer -> State [Integer] () 
factorialProducts 0 = return ()
factorialProducts counter = do
    list <- S.get
    let (prevPart:parts) = list
    let nextPart = prevPart * counter 
    S.put (nextPart : prevPart : parts) 
    factorialProducts (counter - 1)

printFactorialProducts :: Integer -> IO () 
printFactorialProducts n = do
    let products = execState (factorialProducts  n) [1] 
    print products


factorialStateful :: Integer -> StateIO Integer () 
factorialStateful 0 = return ()
factorialStateful n = do
    part <- ST.get
    lift (print part) -- this will now compile as desired 
    ST.put (part * n)
    factorialStateful (n - 1)

printFactorial :: Integer -> IO ()
printFactorial n = evalStateT (factorialStateful n) 1