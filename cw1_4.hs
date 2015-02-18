--- CW1
-- zad 2
--a
silnia 0 = 1
silnia n = n * silnia (n-1)
--b
potega a 0 = 1
potega a n = a * potega a (n-1)
--c
nwd a 0 = a
nwd a b = nwd b (mod a b)
--d
nww a b = div (a*b) (nwd a b)
--e
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
--f
czyTrojkat a b c = if (a+b)>c && (b+c)>a && (a+c)>b then True else False

--- CW2
-- zad 2
--a
przedost [] = error "pusta lista"
przedost [x] = error "za malo elementow"
przedost xs = head(reverse(init xs))
--b
drugielem [] = error "pusta lista"
drugielem [x] = error "za malo elementow"
drugielem xs = head(reverse(init(reverse xs)))
--c
myreverse :: [a] -> [a]
myreverse [] = []
myreverse [x] = [x]
myreverse xs = last xs : myreverse(init xs)
--d
podmiana :: [a] -> [a]
podmiana [] = []
podmiana [x] = [x]
podmiana [a,b] = [b,a]
podmiana (x:xs) = (last xs : init xs) ++ [x]
--e
podmiana2 :: [a] -> [a]
podmiana2 [] = []
podmiana2 [x] = [x]
podmiana2 (x:xx:xs) = (x:head(tail(reverse xs)):init(init xs)) ++ [xx] ++ [last xs]
--f
liczdodatnie :: [Integer] -> Integer
liczdodatnie [] = 0
liczdodatnie xs = toInteger(length(filter (>0) xs))
--g
czyparzysta :: [a] -> Bool
czyparzysta [] = True
czyparzysta xs = if mod (length xs) 2 == 0 then True else False
--h
zamiendrugi a [] = [a]
zamiendrugi a [x] = [x,a]
zamiendrugi a [x,y] = [x,a]
zamiendrugi a (x:xx:xs) = (x:a:xs)

--- CW3
-- zad 2

flatten :: [[a]] -> [a]
flatten xss = foldr (++) [] xss

-- zad 3

count _ [] = 0
count a (x:xs) = if x==a then 1 + count a xs else 0 + count a xs


count2 a xs = length(filter (==a) xs)

-- zad 4
duplicate _ 0 = []
duplicate xs b = xs : duplicate xs (b-1)

-- zad 5
sqrlist [] = []
sqrlist (x:xs) = (^2)x : sqrlist xs

-- zad 6
palindrome [] = True
palindrome [a] = True
palindrome (x:xs) = (x == last(xs)) && palindrome (init(xs)) 

-- zad 7
select _ [] = []
select a (x:xs) = if a == x then xs else [x] ++ select a xs

-- zad 8
delete _ [] = []
delete a (x:xs) = if a == x then delete a xs else [x] ++ delete a xs

-- zad 9
delnth _ [] = []
delnth a (x:xs) = if a == 1 then xs else [x] ++ delnth (a-1) xs

-- zad 10
powerlist [] = [[]]
powerlist (x:xs) = [x:powerl | powerl <- powerlist xs] ++ powerlist xs

--- CW4
-- zad 1
myFoldr f z []     = z
myFoldr f z (x:xs) = f x (myFoldr f z xs)

-- zad 2
myFoldl f z [] = z
myFoldl f z (x:xs) = f (myFoldl f z xs) x

-- zad 3

ciagNR2 (x1:x2:lista) eps = if abs(x2-x1)<=eps then x2 else ciagNR2 lista eps
				
-- zad 4
data Tree a = Empty | Node a (Tree a) (Tree a)

treeMemberPre _ Empty = False
treeMemberPre m (Node a l r)
	| m == a = True
	| otherwise = treeMemberPre m l || treeMemberPre m r
	
treeMemberIn _ Empty = False
treeMemberIn m (Node a l r) = treeMemberIn m l || m == a || treeMemberIn m r

treeMemberPost _ Empty = False
treeMemberPost m (Node a l r) = treeMemberPost m l || treeMemberPost m r || m == a