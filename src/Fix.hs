module Fix where

data Toy' b =
    Output b (Toy' b)
  | Bell (Toy' b)
  | Done