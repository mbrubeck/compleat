import Control.Monad (MonadPlus, mzero, mplus)
-- import Data.List (isPrefixOf)

data Parser a = Parser { parse :: (String -> [(a, String)]) }

instance Monad Parser where
    return a  = Parser (\cs -> [(a, cs)])
    p >>= f   = Parser (\cs -> concat [parse (f a) cs' | (a, cs') <- parse p cs])

instance MonadPlus Parser where
    mzero     = Parser (\cs -> [])
    mplus p q = Parser (\cs -> parse p cs ++ parse q cs)

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\cs -> case parse (p `mplus` q) cs of
                            []     -> []
                            (x:xs) -> [x])

item :: Parser Char
item = Parser (\cs -> case cs of
                        ""     -> []
                        (c:cs) -> [(c, cs)])

sat :: (Char -> Bool) -> Parser Char
sat p = do 
    x <- item
    if p x then return x else mzero

char :: Char -> Parser Char
char c = sat (c==)

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do
    a <- p
    as <- many p
    return (a:as)

-- Bash-specific stuff

token :: Parser String
token = many1 escapedChar

escapedChar :: Parser Char
escapedChar = sat (const True) -- TODO
