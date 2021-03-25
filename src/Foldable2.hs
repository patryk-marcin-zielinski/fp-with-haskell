module Foldable2 where

import Data.Monoid

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF filter = foldMap (\a -> if filter a then pure a else mempty)


-- filterF (\a -> snd a == 2) [(1,2), (4,2), (4,6)] :: Just (Sum Int, Sum Int)
-- 
-- Foldable [] and  a = (,)


sum :: (Foldable t, Num a) => t a -> a
sum = foldr (+) 0

product :: (Foldable t, Num a) => t a -> a
product = foldr (*) 1

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem a ta = getAny $ foldMap (\el -> Any (el == a)) ta 

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' a = foldr (\el acc -> acc || (a == el) ) False

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = ordable (<) 

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = ordable (>)

ordable :: (Foldable t, Ord a) => (a -> a -> Bool) -> t a -> Maybe a
ordable f = foldr (eval f) Nothing
            where 
                eval :: (a -> a -> Bool) -> a -> Maybe a -> Maybe a
                eval _ a Nothing = Just a
                eval fa a (Just b) = if fa a b then Just a else Just b 

null :: (Foldable t) => t a -> Bool
null = undefined 

length :: (Foldable t) => t a -> Int
length = foldr (\_ acc -> acc + 1) 0  

toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []  

-- | Combine the elements of a structure using a monoid.
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = undefined 

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' = undefined 