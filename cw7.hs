import Data.List
poly=[(1,1), (2,2), (3,3), (4,4)]
list1=map fst poly
list2=tail list1 ++ [head list1]
list3=map snd poly
list4=tail list3 ++ [head list3]
parts = zipWith (*) (zipWith (-) list1 list2) (zipWith (+) list3 list4)
area [] = error "to nie jest wielokat"
area [x] = error "punkt nie ma pola"
area [x,y] = error "linia nie ma pola"
area poly = abs ((sum parts) / 2)
				where parts = zipWith (*) (zipWith (-) list1 list2) (zipWith (+) list3 list4)


--- CW 7
--program obliczza pole powierzchni wielokata przy uzyciu wzorow Gaussa
--POLE = 1/2|(SUMA[od i=1 do n] : X_i(Y_i+1 - Y_i-1))|
--funkcja zipWith wykorzystywany jest to utworzenia wszystkich operacji zawartych w nawiasie powyzszego wzoru (zipWith wykonuje zadana funckje parami, biorac kolejno po jednym elemencie z kazdej listy)
-- zad 2
--przyklad dla funkcji sortBy z CW 6 zad 3
odl :: Floating a => (a,a) -> a
odl (x,y) = sqrt((x^2)+(y^2))

porownaj :: (Ord a, Floating a) => (a,a) -> (a,a) -> Ordering
porownaj xs ys
	| odl xs > odl ys = GT
	| otherwise = LT

listaodl2 = \xs -> sortBy porownaj xs

czyListaDoSuma lista suma = not (null (nubBy (\x y -> x+y == suma) lista))

--deleteBy (\x y -> y `mod` x == 0) 4 [6,8,10,12]

--deleteFirstsBy (\x y -> y `mod` x == 0) [4,9,32] [6,8,10,12]

--unionBy (\x y -> x*3==y) [1,2,3,4] [4,6,9,10]

--intersectBy (\x y -> x*x == y) [1,2,3,4] [4,8,12,16,20]

--groupBy (\x y -> (x*y `mod` 3) == 0) [1,2,3,4,5,6,7,8,9]

--xxx a b  | a+b < a*b = LT
--         | otherwise = GT

--insertBy xxx 4 [0,1,3,5,7,9]

--xs = [[1,2],[1,2,3],[2,3]]
--ordLen a b = compare (length a) (length b)
--maximumBy ordLen xs

--minimumBy ordLen xs

-- zad 3
--pusta1 :: [a] -> Bool
--pusta1 x = (x == [])

--funkcja nie jest poprawna, poniewaz funkcja nie ogranicza zbioru mozliwych typow danych w liscie, to takich, na ktorych mozna dokonywac operacji porownania
--poprawna sygnatura funkcji
pusta2 :: Eq a => [a] -> Bool
pusta2 x = (x == [])

-- zad 4
trojki :: Int -> [(Int,Int,Int)]
trojki x = [(a,b,c) | a<-[1..x], b<-[1..a], c<-[1..x], (a^2)+(b^2) == (c^2)]
