--- cw 3
-- zad 1
--a
f1 x y = x+y

f1 :: Num a => a -> a -> a

f2 x y z = if x then y else z

f2 :: Bool -> t -> t -> t

f3 (x,(y,z)) = ((x,y),z)

f3 :: (t1, (t2, t)) -> ((t1, t2), t)

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


sqrlist2 [] = []
sqrlist2 xs = map (^2) xs

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