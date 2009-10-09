import Control.Monad (MonadPlus, mzero, mplus)
-- import Data.List (isPrefixOf)

data Parser a = Parser { parse :: (String -> [(a, String)]) }

instance Monad Parser where
    return a  = Parser (\cs -> [(a, cs)])
    p >>= f   = Parser (\cs -> concat [parse (f a) cs' | (a, cs') <- parse p cs])

instance MonadPlus Parser where
    mzero     = Parser (\cs -> [])
    mplus p q = Parser (\cs -> parse p cs ++ parse q cs)

(<|>) :: Parser a -> Parser a -> Parser a
(<|>) = mplus

item :: Parser Char
item = Parser (\cs -> case cs of
                        ""     -> []
                        (c:cs) -> [(c, cs)])

sat :: (Char -> Bool) -> Parser Char
sat p = do 
    x <- item
    if p x then return x else mzero
