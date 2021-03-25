module Klensi where
    


type Kleisli m a b = a -> m b

(>>>) :: Monad m => Kleisli m a b -> Kleisli m b c -> Kleisli m a c
(f >>> g) a = do 
    b <- f a
    g b

arr :: Monad m => (a -> b) -> Kleisli m a b
arr f = return . f 


readFile' :: Kleisli IO String String
readFile' = undefined 

print' :: Show a => Kleisli IO a ()
print' = undefined 

printFile = readFile' >>> print'


count w = readFile' >>>
    arr words >>> arr (filter (==w)) >>> arr length >>>
    print'


apply :: (Monad m) => (a -> m b) -> (b -> c) -> (a -> m c)
apply f g a = do
  b <- f a
  let c = g b
  return c