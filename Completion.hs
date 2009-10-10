import Control.Monad (mzero, mplus)
import Data.List (isPrefixOf)
import Parser

-- Example

git :: Parser String
git = str "git" --> zeroOrMore gitOptions --> gitCommand

gitOptions :: Parser String
gitOptions = str "--version"
         <|> str "--help"
         <|> str "--work-tree" >> token
         <|> return ""

gitCommand :: Parser String
gitCommand = (str "add" --> zeroOrMore (str "-i" <|> str "-n" <|> str "-v"))
         <|> (str "commit" --> zeroOrMore (str "-m" >> token <|> str "-a" <|> str "--amend"))

-- Completers

complete :: Parser String -> String -> [String]
complete p s = map fst $ parse (apply p) s

(-->) :: Parser a -> Parser a -> Parser a
p --> q = do
    a <- p
    inp <- getInput
    case inp of
        "" -> return a
        _  -> q

str :: String -> Parser String
str s = do
    tok <- token
    inp <- getInput
    case inp of
        "" -> if tok `isPrefixOf` s then return s else mzero
        _  -> if tok == s           then return s else mzero

zeroOrMore :: Parser a -> Parser a
zeroOrMore p = do
    inp <- getInput
    let result = parse p inp
    case result of
        [] -> mzero
        _  -> oneOrMore p

oneOrMore :: Parser a -> Parser a
oneOrMore p = do
    first <- p
    inp <- getInput
    let result = parse p inp
    case result of
        [] -> return first
        _  -> oneOrMore p

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
