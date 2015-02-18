--- cw 4
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