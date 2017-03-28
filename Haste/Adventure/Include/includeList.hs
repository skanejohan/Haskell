module Include.IncludeList where

import Data.List

type IncludeList a = [(a, [a])]

empty :: IncludeList a
empty = []

add :: a -> IncludeList a -> IncludeList a
add a as = (a,[]):as

add_to :: Eq a => a -> a -> IncludeList a -> IncludeList a
add_to a1 cont ((a,as):ass)
    | cont == a = (a,a1:as):ass
    | otherwise = (a,as):(add_to a1 cont ass)
add_to _ _ [] = []

elem_cond :: Eq a => (a -> Bool) -> a -> IncludeList a -> Bool
elem_cond f a' ((a,as):ass)
    | a' == a   = True
    | f a'      = Data.List.elem a' as || elem_cond f a' ass
    | otherwise = elem_cond f a' ass
elem_cond _ _ [] = False

elem :: Eq a => a -> IncludeList a -> Bool
elem = elem_cond (\_ -> True)

delete :: Eq a => a -> IncludeList a -> IncludeList a
delete a' ((a,as):ass)
    | a' == a              = ass
    | Data.List.elem a' as = (a, Data.List.delete a' as):ass
    | otherwise            = (a,as):(Include.IncludeList.delete a' ass)
delete _ [] = []

flatten_cond :: Eq a => (a -> Bool) -> IncludeList a -> [a]
flatten_cond f ((a,as):ass)
    | f a       = a:as ++ flatten_cond f ass
    | otherwise = a:flatten_cond f ass
flatten_cond _ [] = []

flatten :: IncludeList a -> [a]
flatten ((a,as):ass) = a:as ++ flatten ass
flatten []           = []
