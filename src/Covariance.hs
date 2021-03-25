module Covariance where


newtype MakeString a = MakeString { makeString :: a -> String }

showInt :: MakeString Int
showInt = MakeString show