--- cw 5
-- zad 1
liczbaw _ [] n = n
liczbaw elem (g:o) n
	| elem == g = liczbaw elem o (n+1)
	| otherwise = liczbaw elem o n
	
liczbaw2 _ [] = 0
liczbaw2 elem (g:o)
	| elem == g = 1 + liczbaw2 elem o
	| otherwise = 0 + liczbaw2 elem o
-- zad 2	
podzbior l [] = False
podzbior [] l = True
podzbior (g1:o1) (g2:o2)
	| g1==g2 = True && podzbior o1 o2
	| otherwise = False || podzbior (g1:o1) o2

pozbior2 xs ys = and [elem x ys | x<- xs]
	
-- zad 3
iloczyn [] l2 = [] 
iloczyn l2 [] = []
iloczyn (g1:o1) l2 = if elem g1 l2==True then [g1] ++ iloczyn o1 l2 else iloczyn o1 l2

-- zad 4
--a
czyPierwsza n = null [x | x <- [2..(n-1)], mod n x == 0 ]
--b
wypiszPierwsze ile = take ile [x | x <- [2..], czyPierwsza x == True]

-- zad 5
--b
wypiszCzynnikiPierwsze n = [x | x <- [(n),(n-1)..2], mod n x == 0, czyPierwsza x == True]
--a
ileCzynnikowPierwszych n = length (wypiszCzynnikiPierwsze n)

-- zad 6
indeks _ [] = -9999
indeks 1 (g:lista) = g
indeks n (g:lista) = indeks (n-1) (lista)
-- zad 7
data Tree a = Empty | Node a (Tree a) (Tree a)
				deriving(Show)

listaWdrzewo [] = Empty
listaWdrzewo lista = Node (last lista) (listaWdrzewo (init lista)) Empty



