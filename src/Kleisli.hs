module Kleisli where 

newtype Kleisli m a b = Kleisli {runKleisli :: a -> m b}    


class Arrow art where
    arr :: (a -> b) -> art a b
    (>>>) :: arr a b -> art b c -> art a c