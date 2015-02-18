double x = 2 * x
square x = x * x
inc x = x + 1

apply [] x = x
apply (f:fs) x = f (apply fs x)


newtype Parser a = Parser (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (Parser p) input = p input

failure :: Parser Char
failure = Parser (\input -> [])

instance Monad Parser where
    return x = Parser (\input -> [(x, input)])
    (Parser p) >>= f =
        Parser (\input -> case p input of
                             [] -> []
                             [(x, input')] -> case f x of
                                                  Parser p' -> p' input')

item :: Parser Char
item = Parser (\input -> case input of
                            [] -> []
                            (x:xs) -> [(x, xs)])

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else failure

digit :: Parser Char
digit = sat (`elem` ['a'..'z'])

predykat :: Parser Char
predykat = sat (`elem` ['A'..'Z'])

char :: Char -> Parser Char
char c = sat (==c)

Parser p +++ Parser q = Parser $
    \input -> case p input of
                  [] -> q input
                  [(v, out)] -> [(v, out)]

-----------------------------------------------------------

--expr ::= term '+' expr | term
--term ::= factor '*' term | factor
--factor ::= digit | '(' expr ')'
--digit ::= '0' | '1' | ... | '9'
--neg :: = ~predykat | '(' expr ')'

term :: Parser Bool
term = do f <- factor
          do char '^'
             t <- term
             return True
           +++
             return True 

expr :: Parser Bool
expr = do t <- term
          do char 'v'
             e <- expr
             return True
            +++
             return True

factor :: Parser Bool
factor = do d <- predykat
            return (read [d])
           +++
            do char '('
               e <- expr
               char ')'
               return e


impl :: Parser Bool
impl = do t <- term
          do char '>'
             i <- expr
             return True
            +++
             return t

impl :: Parser Bool
eval input = case parse expr input of

              [(n, [])] -> n

              [(_, out)] -> error ("nieskonsumowane " ++ out)

              [] -> error "bledne wejscie"
