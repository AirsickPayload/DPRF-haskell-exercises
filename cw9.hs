-- zad 1
import Data.Char
import Text.Regex.Posix
naInt :: String -> Int
naInt = foldl (\x acc -> digitToInt acc * 10 + x) 0

-- zad 2

class Address a where
	(==) :: a -> a -> Email
	data Email = Local | Domain
	instance Address Email where
		_ == _ = Local
