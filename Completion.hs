import Control.Monad (mzero, mplus)
import Data.List (isPrefixOf)
import Parser

complete :: String -> Parser String
complete s = do
    tok <- token
    if tok `isPrefixOf` s then return s else mzero

-- Tokenize

apply :: Parser a -> Parser a
apply p = spaces >> p

token :: Parser String
token = do
    result <- quotedToken '"' +++ quotedToken '\'' +++ unquotedToken
    spaces
    return result

unquotedToken :: Parser String
unquotedToken = many1 (escaped +++ nonspace)

quotedToken :: Char -> Parser String
quotedToken q = bracket (char q) (many nonQuoteChar) (char q +++ return '_')
    where nonQuoteChar = escaped +++ sat (q/=)

escaped :: Parser Char
escaped = do
    char '\\'
    c <- item
    case c of
        't' -> return '\t'
        'n' -> return '\n'
        'r' -> return '\r'
        _   -> return c
