import Data.List
--CW6
-- zad 1
permutacja :: Eq a => [a] -> [a] -> Bool
permutacja = \xs ys -> and [elem x ys | x <- xs] && length xs == length ys
-- zad 2
listapar :: Eq a => [a] -> [(a,Int)]
listapar = \xs -> [(x,length(filter (==x) xs)) | x <- xs]
-- zad 3

odl :: Floating a => (a,a) -> a
odl (x,y) = sqrt((x^2)+(y^2))

porownaj :: (Ord a, Floating a) => (a,a) -> (a,a) -> Ordering
porownaj xs ys
	| odl xs > odl ys = GT
	| otherwise = LT

listaodl2 = \xs -> sortBy porownaj xs

-- zad 4
gdzieNaLiscie :: Eq a => [a] -> a -> [Int]
gdzieNaLiscie = \lista el -> elemIndices el lista

listaPOM [] _ _ = []
listaPOM (g:lista) el n = if (g==el)==True then [n] ++ listaPOM lista el (n+1) else listaPOM lista el (n+1)

gdzielista :: Eq a => [a] -> a -> [Int]
gdzielista = \(g:lista) el -> listaPOM (g:lista) el 0


-- zad 5
choose n 0 = 1
choose 0 k = 0
choose n k = choose (n-1) (k-1) * n `div` k

pascalL = \m -> if m==0 then [[1]] else [x | x <- pascalL (m-1)] ++ [[choose m k | k <- [0,1..m]]]

-- zad 6

--zad3
polacz x [] = x
polacz [] x = x
polacz (x:xs) (y:ys) = if x < y
                          then x:(polacz xs (y:ys))
                          else y:(polacz (x:xs) ys)

polacz2 x [] = x
polacz2 [] x = x
polacz2 (x:xs) (y:ys) = if x < y
                          then x:(\xs (y:ys) -> polacz xs (y:ys)) xs (y:ys)
                          else y:(\(x:xs) ys -> polacz (x:xs) ys) (x:xs) ys

--zad4

data Tree a = Empty | Node a (Tree a) (Tree a)
				deriving(Show)

prawyIlewy (Node _ Empty Empty) = []
prawyIlewy (Node _ l Empty) = [l]
prawyIlewy (Node _ Empty r) = [r]
prawyIlewy (Node _ l r) = [l,r]

wartoscWierzcholka (Node a _ _) = a

poziomo drzewo = poz [drzewo]

poziomo2 = \drzewo -> poz [drzewo]

poz [] = []
poz drzewo = map wartoscWierzcholka drzewo ++ poz (concat (map prawyIlewy drzewo))

poz2 = \drzewo -> map wartoscWierzcholka drzewo ++ poz2 (concat (map prawyIlewy drzewo))
