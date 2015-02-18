--zad 1
x >> y = x >>= const y

zad3  :: IO()
zad3 = do
            putStr "Pierwsza liczba: "
            l1 <- readLn
            putStr "Druga liczba: "
            l2 <- readLn
            putStrLn "-----"
            putStr "Suma: "
            print (l1+l2)
            putStr "Iloczyn: "
            print (l1*l2)
            putStrLn "Roznica: "
            print (l1-l2)


zad4 = do
        putStr "Imie: "
        imie <- getLine
        putStr "Nazwisko: "
        nazwisko <- getLine
        putStrLn "-------------"
        putStr (take 1 imie)
        putStr "."
        putStrLn (take 1 nazwisko)
