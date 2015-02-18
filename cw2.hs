potega x n = if n == 0 then 1 else if x == 0 then 0 else x * potega x (n-1)

potega2 x n = potegaPOM x n 1
            where potegaPOM 0 n wyn = 0
                  potegaPOM x 0 wyn = wyn
                  potegaPOM x n wyn = potegaPOM x (n-1) (wyn*x)
				  
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