import Control.Monad (mzero, mplus)
import Data.List (isPrefixOf)
import Parser

-- Example

git :: Completer
git = str "git" --> rpt gitOptions --> gitCommand

gitOptions :: Completer
gitOptions = str "--version" <|> str "--help" <|> str "--work-tree"

gitCommand :: Completer
gitCommand = (str "add" --> rpt (str "-i" <|> str "-n" <|> str "-v"))
         <|> (str "commit" --> rpt (str "-m" <|> str "-a" <|> str "--amend"))

-- Completers

type Completer = Parser [String]

complete :: Completer -> String -> [String]
complete p s = concat $ map fst $ parse (apply p) s

(-->) :: Parser a -> Parser a -> Parser a
p --> q = do
    a <- p
    inp <- getInput
    case inp of
        "" -> return a
        _  -> q

rpt :: Parser a -> Parser a
rpt p = foldr1 (-->) (repeat p)

str :: String -> Completer
str s = do
    tok <- token
    inp <- getInput
    case inp of
        "" -> if tok `isPrefixOf` s then return [s] else return []
        _  -> if tok == s           then return [s] else mzero

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
