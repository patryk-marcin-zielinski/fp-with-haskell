{-# LANGUAGE DerivingVia, DeriveFunctor, GeneralizedNewtypeDeriving #-}

module Recursive where

import Data.Functor   

data Op = Add | Mult

data ASTF r =
    BinOpF Op r r 
    | NumF Int
    deriving (Functor)