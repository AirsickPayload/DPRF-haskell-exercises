-- zad 1
import Text.Printf
printf "%.10f\n" (1/7)

module Mymodule where
    import Data.List

    mymember [] _ = True
    mymember _ [] = False
    mymember e (g:o) = if e == g then True else mymember e o

    mysubset xs ys = and [elem x ys | x<- xs]

    myunion as bs = foldl (\xs y -> if elem y xs then xs else xs ++ [y]) as bs

    myintersection [] l2 = []
    myintersection l2 [] = []
    myintersection (g1:o1) l2 = if elem g1 l2==True then [g1] ++ myintersection o1 l2 else myintersection o1 l2

    mydifference l1 l2 = foldl (\xs y -> if elem y xs then delete y xs else xs) l1 l2

    mysymdifference l1 l2 = myunion (l1 \\ l2) (l2 \\ l1)

    mydelete e xs = filter (/= e) xs
