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

applyTwice f x = f (f x)  